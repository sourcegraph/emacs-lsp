;; Copyright (c) 2016 Sourcegraph Inc

;; This minor mode handles sending requests to a language server and rendering
;; responses.

(require 'projectile)
(require 'lsp)
(require 'cl-lib)
(require 'button)

(defstruct ls-connection
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
  (y-or-n-p body)
  (let ((buf (get-buffer-create "*LSP-Error*")))
    (with-current-buffer buf
      (insert body)
      (insert "\n"))))

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
        (ignore-errors
          ;; FIXME: Expected error: invalid json should dump that portion of the
          ;; buffer, instead of ignoring. Right now, invalid json will cause
          ;; *whole* buffer to be dumped

          ;; Expected error: args-out-of-range from buffer-substring should
          ;; break from the loop, to be tried again the next time the filter
          ;; function is called, once there is more data.
          (while (string-equal "Content-Length: " (buffer-substring (point-min) 17))
            (save-excursion
              ;; extract the content-length
              ;; TODO: replace with principled header parsing into alist
              (goto-char 17)
              (looking-at "[0-9]+")
              (let ((content-length (string-to-int (match-string 0)))
                    (conn (gethash ws-cache lsp-ws-connection-map)))
                ;; scan point forward to start of body
                (search-forward "\r\n{")
                (let* ((content-body (buffer-substring (- (point) 1) content-length))
                       (content (json-read-from-string content-body))
                       (sess (ls-connection-session conn))
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
                      (remhash id sess))))))))))))


;;; FIXME: this should probably use a while loop of some sort, instead of the
;;; recursive call to init-lsp-conn-inner, although it's a bit challenging due
;;; to the callback nature.

(defun init-lsp-conn-inner (ws conn)
    (send-lsp-msg (lsp-init (lsp-init-params nil nil ws nil))
                      (lambda (res body)
                        (cond ((equal res :error)
                               (let* ((err (alist-get 'error body))
                                      (retry (alist-get 'retry err)))
                                 (lsp-display-err-and-wait-for-confirmation body)
                                 (init-lsp-conn-inner ws conn)))
                              ((equal res :success)
                               (setq (ls-connection-server-caps conn) (alist-get 'capabilities body)))))))

(defun lsp-mode-init-conn (host port)
  "Initialize a new connection to an LSP"
  (interactive "Mhostname:\nnport:")
  (let* ((bufname (concat "*lsp*" (int-to-string (random))))
         (net-proc (open-network-stream "lsp" bufname host port :type 'plain ))
         (session (make-hash-table))
         (ws (projectile-project-root))
         (conn (make-ls-connection :process net-proc :session session :bufname bufname))
         )
    (set-process-filter net-proc (lsp-filter session))
    (puthash ws conn lsp-ws-connection-map)
    (init-lsp-conn-inner ws conn)
    (send-lsp-msg (lsp-did-open-text-doc (lsp-text-doc-id (uri-for-path (buffer-file-name)))) 'lsp-ignore)
    ))

(defvar-local ws-cache (projectile-project-root))

(defun send-lsp-msg (req &optional cb)
  (let ((s (gethash ws-cache lsp-ws-connection-map))
        (nid (abs (random (- (expt 2 64) 1)))))
    (puthash nid cb (ls-connection-session s))
    (process-send-string (ls-connection-process s) (lsp-message (cons `(id . ,nid) req)))
    )
  )

(defun uri-for-path (p)
  (message p)
  (apply 'concat (list "file://" p)) ; FIXME: more robust? does this work on windows?
  )

(defun lsp-mode-find-file-hook ()
  (send-lsp-msg (lsp-did-open-text-doc (lsp-text-doc-id (uri-for-path (buffer-file-name)))) 'lsp-ignore))

(defun lsp-mode-write-file-functions ()
  (send-lsp-msg (lsp-did-save-text-doc (lsp-text-doc-id (uri-for-path (buffer-file-name)))) 'lsp-ignore)
  nil)

(defun current-lsp-pos ()
  (lsp-position (line-number-at-pos (point)) (current-column)))

(defun current-lsp-text-doc-pos ()
  (lsp-text-doc-pos-params (lsp-text-doc-id (uri-for-path (buffer-file-name))) (current-lsp-pos)))

(defun lsp-mode-goto-cb (res body)
  (cond (((equal res :error)
          (lsp-ignore res body))
         ((equal res :success)
          (if (listp body)
              (lsp-mode-select-destination body)
            (lsp-mode-goto-loc body))))))

(defun alist-navigate (obj &rest path)
  "Extract a property from an alist, using successive symbols from path."
  (let ((cur_obj obj))
    (dolist (cur path cur_obj)
      (setq cur_obj (alist-get cur cur_obj)))))

(defun lsp-mode-goto-loc (loc)
  (let ((comps (split-string (alist-get 'uri loc) "://")))
    (if (equal (car comps) "file")
        (let ((newbuf (find-file-noselect (substring (alist-get 'uri loc) 8))))
          (switch-to-buffer newbuf)
          (lsp-mode)
          (forward-line (alist-navigate loc 'range 'start 'line))
          (forward-char (alist-navigate loc 'range 'start 'character)))
      )))

(defun lsp-mode-select-destination (locs)
  "Given a list of locations, display them in a new window with hyperlinks"
  (let ((buf (get-buffer-create "*LSP-Select-Destination*")))
    (with-current-buffer buf
      (dolist (loc locs)
        (insert-button
         (apply 'concat (list (alist-get 'uri loc) " at line " (int-to-string (alist-navigate loc 'range 'start 'line))))
         :location loc
         :action (lambda (b) (lsp-mode-goto-loc (button-get b :location))))))
    (switch-to-buffer-other-window buf)))

(defun lsp-mode-list-symbols (syms)
  "Given a list of symbols, display them in a new window with hyperlinks"
  (let ((buf (get-buffer-create "*LSP-List-Symbols*")))
    (with-current-buffer buf
      (dolist (sym syms)
        (insert-button
         (apply 'concat (list
                         (symbol-name (car (rassq (alist-get 'kind sym) lsp-symbol-kinds)))
                         " "
                         (alist-get 'name sym)
                         " in "
                         (alist-navigate sym 'location 'uri)
                         " at line "
                         (int-to-string (alist-navigate sym 'location 'range 'start 'line))))
         :symbol sym
         :action (lambda (b) (lsp-mode-goto-loc (alist-get 'location (button-get b :symbol))))
         )))
    (switch-to-buffer-other-window buf)))

(defun lsp-mode-goto ()
  "Go to the definition of the symbol near point"
  (interactive)
  (send-lsp-msg (lsp-goto-def (current-lsp-text-doc-pos)) 'lsp-mode-goto-cb))

(defun lsp-mode-hover-cb (res body)
  (cond (((equal res :error)
          (lsp-ignore res body))
         ((equal res :success)
          (let ((message
                 (if (listp (alist-get 'contents body))
                     (apply 'concat (alist-get 'contents body))
                   (alist-get 'contents body))))
            (display-message-or-buffer message "*LSP-Message*"))))))

(defun lsp-mode-hover ()
  "Display hover information for the symbol near point"
  (interactive)
  (send-lsp-msg (lsp-hover (current-lsp-text-doc-pos)) 'lsp-mode-hover-cb))

(defun lsp-mode-references-cb (res body)
  (cond (((equal res :error)
          (lsp-ignore res body))
         ((equal res :success)
          (lsp-mode-select-destination body)))))

(defun lsp-mode-references ()
  "Find references to the symbol near point"
  (interactive)
  (send-lsp-msg (lsp-find-refs (lsp-ref-params (current-lsp-text-doc-pos)
                                                   (lsp-ref-context json-false)))
                    'lsp-mode-references-cb))

(defun lsp-mode-symbol-cb (res body)
  (cond (((equal res :error)
          (lsp-ignore res body))
         ((equal res :success)
          (lsp-mode-list-symbols body)))))

(defun lsp-mode-symbol (query)
  "Search for project-wide symbols matching the query string"
  (interactive "Mquery:")
  (send-lsp-msg (lsp-workspace-symbols (lsp-workspace-symbol-params query)) 'lsp-mode-symbol-cb))

(define-minor-mode lsp-mode
  "Use a Language Server to provide semantic information about your code"
  :lighter " lsp"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-l i") 'lsp-mode-init-conn)
            (define-key map (kbd "C-c C-l g") 'lsp-mode-goto)
            (define-key map (kbd "C-c C-l h") 'lsp-mode-hover)
            (define-key map (kbd "C-c C-l r") 'lsp-mode-references)
            (define-key map (kbd "C-c C-l s") 'lsp-mode-symbol)
            map)
  (if lsp-mode
      (progn
        (add-hook 'find-file-hook 'lsp-mode-find-file-hook nil t)
        (add-hook 'foo 'lsp-foo nil t)
        (add-hook 'foo 'lsp-foo nil t)
        )
    (remove-hook 'foo 'lsp-foo t)
    (remove-hook 'foo 'lsp-foo t)
    (remove-hook 'foo 'lsp-foo t)
    )
  )
