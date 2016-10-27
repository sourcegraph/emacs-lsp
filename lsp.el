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

(defun lsp-completion (id pos-params)
  (lsp-request id "textDocument/completion" pos-params))

(defun lsp-resolve-completion (id completion-item)
  (lsp-request id "completionItem/resolve" completion-item))

(defun lsp-hover (id pos-params)
  (lsp-request id "textDocument/hover" pos-params))

(defun lsp-signature-help (id pos-params)
  (lsp-request id "textDocument/signatureHelp" pos-params))

(defun lsp-goto-def (id pos-params)
  (lsp-request id "textDocument/definition" pos-params))

(defun lsp-ref-context (include-decl)
  `((includeDeclaration . ,include-decl)))

(defun lsp-ref-params (pos-params context)
  (cons `(context . ,context) context))

(defun lsp-find-refs (id ref-params)
  (lsp-request id "textDocument/references" ref-params))

(defun lsp-highlights (id pos-params)
  (lsp-request id "textDocument/documentHighlight" pos-params))

(defun lsp-symbol-params (doc-id)
  `((textDocument . ,doc-id)))

(defvar lsp-symbol-kinds
  '(
    (file . 1) (module . 2) (namespace . 3) (package . 4) (class . 5) (method . 6)
    (property . 7) (field . 8) (constructor . 9) (enum . 10) (interface . 11)
    (function . 12) (variable . 13) (constant . 14) (string . 15) (number . 16)
    (boolean . 17) (array . 18)
    ))

(defun lsp-symbols (id symbol-params)
  (lsp-request id "textDocument/documentSymbol" symbol-params))

(defun lsp-workspace-symbol-params (query)
  `((query . ,query)))

(defun lsp-workspace-symbols (id symbol-params)
  (lsp-request id "workspace/symbol" symbol-params))

(defun lsp-code-action-context (diagnostics)
  `((diagnostics . ,diagnostics)))

(defun lsp-code-action-params (doc-id range action-context)
  `((textDocument . ,doc-id) (range . ,range) (context . ,action-context)))

(defun lsp-code-action (id action-params)
  (lsp-request id "textDocument/codeAction" action-params))

(defun lsp-code-lens-params (doc-id)
  `((textDocument . ,doc-id)))

(defun lsp-code-lens (id code-lens-params)
  (lsp-request id "textDocument/codeLens" code-lens-params))
