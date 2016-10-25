;; Copyright (c) 2016 Sourcegraph Inc

;; This package has helper functions for parsing and unparsing LSP messages.

(require 'json)

(defun lsp-wrap-payload (body &optional content-type)
  "Add Content-Type and Content-Length headers to an LSP payload"
  (let ((len (length body))
        (type (if (null content-type) "Content-Type: application/vscode-jsonrpc; charset=utf-8")))
    (apply #'concat (mapcar (lambda (s) (encode-coding-string s 'utf-8))
                            `("Content-Length: " ,(number-to-string len) "\r\n" "Content-Type: " ,type "\r\n\r\n" ,body)))
    )
  )

(defun lsp-request (id method &optional params)
  `((id . ,id) (method . ,method) (params . ,params))
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

(defun lsp-init-params (caps &optional ppid root-path init-options)
  `((capabilities . ,caps) (processId . ,ppid) (rootPath . ,root-path) (initializationOptions . ,init-options)))

(defun lsp-shutdown (id)
  (lsp-request id "shutdown"))

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

(defun lsp-save-text-doc (doc-id)
  (lsp-ntfn "textDocument/didSave" `((textDocument . ,doc-id))))

(defvar lsp-file-change-type
  '((created . 1) (changed . 2) (deleted . 3)))

(defun lsp-file-event (uri type)
  `((uri . ,uri) (type . ,(alist-get type lsp-file-change-type))))

(defun lsp-did-change-watched-files (file-events)
  (lsp-ntfn "workspace/didChangeWatchedFiles" `((changes . ,file-events))))
