# emacs-lsp

[LSP](https://github.com/Microsoft/language-server-protocol) minor mode for
Emacs.

This mode will hook into [company-mode](https://company-mode.github.io/) to
provide auto-completion.

# Using

Activate `lsp-mode` in a buffer. Then, press `C-c C-l i` to initialize a
connection to an LSP server. Only connections over TCP are supported right
now.

Each LSP connection is associated with a projectile-project-root.  `lsp-mode`
will try to use the connection for the project corresponding to the current
buffer.

Commands:

`C-c C-l i` - initialize a new connection

`C-c C-l g` - go to the definition of the symbol near point

`C-c C-l h` - display hover text for the location at point

`C-c C-l r` - find references to the symbol near point

`C-c C-l s` - search for symbols in the workspace

`C-c C-l q` - shutdown the LSP server

## License

MIT
