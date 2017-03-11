;;; lsp-mode.el --- Support for talking to an LSP server to provide semantic information about programs

;; Copyright (c) 2016 Sourcegraph Inc

;; Author: Corey Richardson <corey@octayn.net>
;; Keywords: convenience
;; Package-Requires: ((projectile "0.14.0") (emacs "25.1"))

;;; Commentary:

;; Activating `lsp-mode' in a buffer enables LSP server communication
;; support.  Use `lsp-mode-init-conn' to initialize a connection to an
;; LSP server.  Only connections over TCP are supported right now.
;; Each LSP connection is associated with a `projectile-project-root':
;; lsp-mode will try to use the connection for the project
;; corresponding to the current buffer.

;; See `lsp-mode-map' for further commands.

;;; Code:

(require 'projectile)
(require 'cl-lib)
(require 'button)
(require 'json)

;;; LSP datatype construction

(defun lsp-wrap-payload (body &optional content-type)
  "Add Content-Type and Content-Length headers to an LSP payload"
  (let ((len (length body))
        (type (or content-type "application/vscode-jsonrpc; charset=utf-8")))
    (mapconcat (lambda (s) (encode-coding-string s 'utf-8))
               `("Content-Length: " ,(number-to-string len) "\r\n" "Content-Type: " ,type "\r\n\r\n" ,body)
               "")))

(defun lsp-message (msg)
  (lsp-wrap-payload (json-encode msg)))

(defun lsp-request (method &optional params)
  `((method . ,method) (params . ,params))
  )

(defun lsp-ntfn (method &optional params)
  `((method . ,method) (params . ,params)))

(defun lsp-cancel (id)
  `((method . "$/cancelRequest") (id . ,id)))

(defun lsp-position (line character)
  `((line . ,line) (character . ,character)))

(defun lsp-range (start end)
  `((start . ,start) (end . ,end)))

(defun lsp-location (uri range)
  `((uri . ,uri) (range . ,range)))

(defvar lsp-diagnostic-severity
  '((error . 1) (warning . 2) (info . 3) (hint . 4)))

(defvar lsp-message-type
  '((error . 1) (warning . 2) (info . 3) (log . 4)))

(defun lsp-diagnostic (range message &optional severity code source)
  `((range . ,range) (message . ,message) (severity . ,(alist-get severity lsp-diagnostic-severity)) (code . ,code) (source . ,source)))

(defun lsp-command (title command &optional arguments)
  `((title . ,title) (command . ,command) (arguments . ,arguments)))

(defun lsp-textedit (range newText)
  `((range . ,range) (newText . ,newText)))

(defun lsp-workspace-edit (changes)
  `((changes . ,changes)))

(defun lsp-text-doc-id (uri)
  `((uri . ,uri)))

(defun lsp-text-doc-item (uri lang-id version text)
  `((uri . ,uri) (languageId . ,lang-id) (version ,version) (text . ,text)))

(defun lsp-versioned-text-doc-item (doc-id version)
  (cons `(version . ,version) doc-id))

(defun lsp-text-doc-pos-params (doc-id pos)
  `((textDocument . ,doc-id) (position . ,pos)))

(defun lsp-init (init-params)
  (lsp-request "initialize" init-params))

(defun lsp-init-params (caps &optional ppid root-path init-options)
  `((capabilities . ,caps) (processId . ,ppid) (rootPath . ,root-path) (initializationOptions . ,init-options)))

(defun lsp-shutdown ()
  (lsp-request "shutdown"))

(defun lsp-exit ()
  (lsp-ntfn "exit"))

(defun lsp-show-msg (type msg)
  (lsp-ntfn "window/showMessage" `((type . ,(alist-get type lsp-message-type)) (message . ,msg))))

(defun lsp-did-config-change (args)
  (lsp-ntfn "workspace/didChangeConfiguration" `(settings . ,args)))

(defun lsp-did-open-text-doc (doc-item)
  (lsp-ntfn "textDocument/didOpen" `((textDocument . ,doc-item))))

(defun lsp-text-doc-content-change (text &optional range rangeLength)
  `((range . ,range) (rangeLength . ,rangeLength) (text . ,text)))

(defun lsp-did-change-text-doc (versioned-doc-id content-changes)
  (lsp-ntfn "textDocument/didChange" `((textDocument . ,versioned-id) (contentChanges . ,content-changes))))

(defun lsp-did-close-text-doc (doc-id)
  (lsp-ntfn "textDocument/didClose" `((textDocument . ,doc-id))))

(defun lsp-did-save-text-doc (doc-id)
  (lsp-ntfn "textDocument/didSave" `((textDocument . ,doc-id))))

(defvar lsp-file-change-type
  '((created . 1) (changed . 2) (deleted . 3)))

(defun lsp-file-event (uri type)
  `((uri . ,uri) (type . ,(alist-get type lsp-file-change-type))))

(defun lsp-did-change-watched-files (file-events)
  (lsp-ntfn "workspace/didChangeWatchedFiles" `((changes . ,file-events))))

(defun lsp-completion (pos-params)
  (lsp-request "textDocument/completion" pos-params))

(defun lsp-resolve-completion (completion-item)
  (lsp-request "completionItem/resolve" completion-item))

(defun lsp-hover (pos-params)
  (lsp-request "textDocument/hover" pos-params))

(defun lsp-signature-help (pos-params)
  (lsp-request "textDocument/signatureHelp" pos-params))

(defun lsp-goto-def (pos-params)
  (lsp-request "textDocument/definition" pos-params))

(defun lsp-ref-context (include-decl)
  `((includeDeclaration . ,include-decl)))

(defun lsp-ref-params (pos-params context)
  (cons `(context . ,context) pos-params))

(defun lsp-find-refs (ref-params)
  (lsp-request "textDocument/references" ref-params))

(defun lsp-highlights (pos-params)
  (lsp-request "textDocument/documentHighlight" pos-params))

(defun lsp-symbol-params (doc-id)
  `((textDocument . ,doc-id)))

(defvar lsp-symbol-kinds
  '(
    (file . 1) (module . 2) (namespace . 3) (package . 4) (class . 5) (method . 6)
    (property . 7) (field . 8) (constructor . 9) (enum . 10) (interface . 11)
    (function . 12) (variable . 13) (constant . 14) (string . 15) (number . 16)
    (boolean . 17) (array . 18)
    ))

(defun lsp-symbols (symbol-params)
  (lsp-request "textDocument/documentSymbol" symbol-params))

(defun lsp-workspace-symbol-params (query)
  `((query . ,query)))

(defun lsp-workspace-symbols (symbol-params)
  (lsp-request "workspace/symbol" symbol-params))

(defun lsp-code-action-context (diagnostics)
  `((diagnostics . ,diagnostics)))

(defun lsp-code-action-params (doc-id range action-context)
  `((textDocument . ,doc-id) (range . ,range) (context . ,action-context)))

(defun lsp-code-action (action-params)
  (lsp-request "textDocument/codeAction" action-params))

(defun lsp-code-lens-params (doc-id)
  `((textDocument . ,doc-id)))

(defun lsp-code-lens (code-lens-params)
  (lsp-request "textDocument/codeLens" code-lens-params))

;;; LS protocol handling.

(cl-defstruct lsp-connection
  process
  session
  bufname
  exit-hooks
  server-caps
  )

(defvar lsp-ws-connection-map (make-hash-table))

;;; Requests are buffered until at least one complete request is received. This
;;; is checked by scanning for Content-Length from the start of the buffer
;;; whenever the filter function is called. If at least that many bytes are
;;; present, the body of the request is parsed out into an object and inserted
;;; into the session for the ID noted in the object. If there is a callback
;;; registered, it will be called with the object.

(defun lsp-ignore (res body)
  (message "Ignored LSP error: %S" body))

(defun lsp-filter (session)
  (lambda (proc string)
    (when (buffer-live-p (process-buffer proc))
      (with-current-buffer (process-buffer proc)
        (let ((moving (= (point) (process-mark proc))))
          (save-excursion
            ;; Insert the text, advancing the process marker.
            (goto-char (process-mark proc))
            (insert string)
            (set-marker (process-mark proc) (point)))
          (if moving (goto-char (process-mark proc))))
        ;; FIXME: Expected error: invalid json should dump that portion of the
        ;; buffer, instead of ignoring. Right now, invalid json will cause
        ;; all future messages to be ignored, as it will die trying to parse
        ;; the firstmost message.

        ;; Expected error: args-out-of-range from buffer-substring should
        ;; break from the loop, to be tried again the next time the filter
        ;; function is called, once there is more data.

        (condition-case error
            (while (string-equal "Content-Length: " (buffer-substring (point-min) 17))
              (save-excursion
                ;; extract the content-length
                ;; TODO: replace with principled header parsing into alist
                (goto-char 17)
                (looking-at "[0-9]+")
                (let ((content-length (string-to-int (match-string 0)))
                      (conn (gethash (lsp-ws-cache-key) lsp-ws-connection-map)))
                  ;; scan point forward to start of body
                  (search-forward "\r\n{")
                  (let* ((content-body (buffer-substring (- (point) 1) (+ (- (point) 1) content-length)))
                         (content (let ((json-array-type 'list)) (json-read-from-string content-body)))
                         (sess (lsp-connection-session conn))
                         (id (alist-get 'id content))
                         (cb (gethash id sess)))
                    ;; remove the request from the buffer
                    (delete-region (point-min) (+ (- (point) 1) content-length))
                    ;; handle the request body
                    (if (null cb)
                        (puthash id content sess)
                      (progn
                        (if (null (alist-get 'result content))
                            (apply cb (list :error (alist-get 'error content)))
                          (apply cb (list :success (alist-get 'result content))))
                        (remhash id sess)))))))
          (args-out-of-range t)
          )))))


;;; FIXME: this should probably use a while loop of some sort, instead of the
;;; recursive call to init-lsp-conn-inner, although it's a bit challenging due
;;; to the callback nature.

(defun lsp-init-conn-inner (ws conn)
  (lsp-send-msg (lsp-init (lsp-init-params nil nil ws nil))
                (lambda (res body)
                  (cond ((equal res :error)
                         (let* ((err (alist-get 'error body))
                                (retry (alist-get 'retry err)))
                           (lsp-display-err-and-wait-for-confirmation body)
                           (lsp-init-conn-inner ws conn)))
                        ((equal res :success)
                         (setf (lsp-connection-server-caps conn) (alist-get 'capabilities body)))))))

(defun lsp-mode-init-conn (host port)
  "Initialize a new connection to an LSP"
  (interactive "Mhostname:\nnport:")
  (let* ((bufname (concat "*lsp*" (int-to-string (random))))
         (net-proc (open-network-stream "lsp" bufname host port :type 'plain ))
         (session (make-hash-table))
         (ws (projectile-project-root))
         (conn (make-lsp-connection :process net-proc :session session :bufname bufname))
         )
    (set-process-filter net-proc (lsp-filter session))
    (puthash ws conn lsp-ws-connection-map)
    (lsp-init-conn-inner ws conn)
    (lsp-send-msg (lsp-did-open-text-doc (lsp-buffer-text-doc-id)) 'lsp-ignore)
    ))

(defun lsp-ws-cache-key ()
  (projectile-project-root))

(defun lsp-send-msg (req &optional cb)
  (let ((s (gethash (lsp-ws-cache-key) lsp-ws-connection-map))
        (nid (abs (random (- (expt 2 64) 1)))))
    (puthash nid cb (lsp-connection-session s))
    (process-send-string (lsp-connection-process s) (lsp-message (cons `(id . ,nid) req)))
    )
  )

(defun lsp-uri-for-path (p)
  (concat "file://" p)) ; FIXME: more robust? does this work on windows?

(defun lsp-buffer-text-doc-id (&optional buffer)
  "Return the text doc ID for BUFFER, or the current buffer if not supplied."
  (lsp-text-doc-id (lsp-uri-for-path (buffer-file-name buffer))))

(defun lsp-mode-find-file-hook ()
  (lsp-send-msg (lsp-did-open-text-doc (lsp-buffer-text-doc-id)) 'lsp-ignore))

(defun lsp-mode-write-file-functions ()
  (lsp-send-msg (lsp-did-save-text-doc (lsp-buffer-text-doc-id)) 'lsp-ignore)
  nil)

(defun lsp-current-lsp-pos ()
  (lsp-position (line-number-at-pos (point)) (current-column)))

(defun lsp-current-lsp-text-doc-pos ()
  (lsp-text-doc-pos-params (lsp-buffer-text-doc-id) (lsp-current-lsp-pos)))

(defun lsp-alist-navigate (obj &rest path)
  "Extract a property from an alist, using successive symbols from path."
  (let ((cur_obj obj))
    (dolist (cur path cur_obj)
      (setq cur_obj (alist-get cur cur_obj)))))

(defun lsp-mode-goto-loc (loc)
  (let ((comps (split-string (alist-get 'uri loc) "://")))
    (if (equal (car comps) "file")
        (let ((newbuf (find-file-noselect (substring (alist-get 'uri loc) 7))))
          (switch-to-buffer-other-window newbuf)
          (lsp-mode)
          (beginning-of-buffer)
          (forward-line (lsp-alist-navigate loc 'range 'start 'line))
          (forward-char (lsp-alist-navigate loc 'range 'start 'character)))
      )))

(defun lsp-mode-select-destination (locs)
  "Given a list of locations, display them in a new window with hyperlinks"
  (let ((buf (get-buffer-create "*LSP-Select-Destination*")))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (loc locs)
        (insert-button
         (apply 'concat (list (alist-get 'uri loc) " at line " (int-to-string (lsp-alist-navigate loc 'range 'start 'line)) "\n"))
         'location loc
         'follow-link t
         'action (lambda (b) (lsp-mode-goto-loc (button-get b 'location))))))
    (switch-to-buffer-other-window buf)))

(defun lsp-mode-list-symbols (syms)
  "Given a list of symbols, display them in a new window with hyperlinks"
  (let ((buf (get-buffer-create "*LSP-List-Symbols*")))
    (with-current-buffer buf
      (erase-buffer)
      (dolist (sym syms)
        (insert-button
         (apply 'concat (list
                         (symbol-name (car (rassq (alist-get 'kind sym) lsp-symbol-kinds)))
                         " "
                         (alist-get 'name sym)
                         " in "
                         (lsp-alist-navigate sym 'location 'uri)
                         " at line "
                         (int-to-string (lsp-alist-navigate sym 'location 'range 'start 'line))
                         "\n"))
         'symbol sym
         'follow-link t
         'action (lambda (b) (lsp-mode-goto-loc (alist-get 'location (button-get b 'symbol))))
         )))
    (switch-to-buffer-other-window buf)))

(defun lsp-mode-goto-cb (res body)
  (cond ((equal res :error)
          (lsp-ignore res body))
         ((equal res :success)
          (if (alist-get 'uri body)
              (lsp-mode-goto-loc body)
              (lsp-mode-select-destination body)))))

(defun lsp-mode-goto ()
  "Go to the definition of the symbol near point"
  (interactive)
  (lsp-send-msg (lsp-goto-def (lsp-current-lsp-text-doc-pos)) 'lsp-mode-goto-cb))

(defun lsp-mode-hover-cb (res body)
  (cond ((equal res :error)
          (lsp-ignore res body))
        ((equal res :success)
          (let* ((body
                  (if (sequencep (alist-get 'contents body))
                      (alist-get 'contents body)
                    (list (alist-get 'contents body))))
                 (message (mapcar (lambda (m) (if (listp m) (alist-get 'value m) m)) body)))
            (display-message-or-buffer (apply 'concat message) "*LSP-Hover*")))))

(defun lsp-mode-hover ()
  "Display hover information for the symbol near point"
  (interactive)
  (lsp-send-msg (lsp-hover (lsp-current-lsp-text-doc-pos)) 'lsp-mode-hover-cb))

(defun lsp-mode-references-cb (res body)
  (cond ((equal res :error)
          (lsp-ignore res body))
         ((equal res :success)
          (lsp-mode-select-destination (if (alist-get 'uri body) (list body) body)))))

(defun lsp-mode-references ()
  "Find references to the symbol near point"
  (interactive)
  (lsp-send-msg (lsp-find-refs (lsp-ref-params (lsp-current-lsp-text-doc-pos)
                                               (lsp-ref-context json-false)))
                'lsp-mode-references-cb))

(defun lsp-mode-symbol-cb (res body)
  (cond ((equal res :error)
          (lsp-ignore res body))
         ((equal res :success)
          (lsp-mode-list-symbols body))))

(defun lsp-mode-symbol (query)
  "Search for project-wide symbols matching the query string"
  (interactive "Mquery:")
  (lsp-send-msg (lsp-workspace-symbols (lsp-workspace-symbol-params query)) 'lsp-mode-symbol-cb))

(defun lsp-mode-shutdown ()
  (interactive)
  (lsp-send-msg (lsp-shutdown) 'lsp-ignore))

(define-minor-mode lsp-mode
  "Use a Language Server to provide semantic information about your code"
  :lighter " lsp"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-l i") 'lsp-mode-init-conn)
            (define-key map (kbd "C-c C-l g") 'lsp-mode-goto)
            (define-key map (kbd "C-c C-l h") 'lsp-mode-hover)
            (define-key map (kbd "C-c C-l r") 'lsp-mode-references)
            (define-key map (kbd "C-c C-l s") 'lsp-mode-symbol)
            (define-key map (kbd "C-c C-l q") 'lsp-mode-shutdown)
            map)
  (if lsp-mode
      (progn
        (add-hook 'find-file-hook 'lsp-mode-find-file-hook nil t)
        )
    (remove-hook 'find-file-hook 'lsp-mode-find-file-hook t)
    )
  )

(provide 'lsp-mode)
