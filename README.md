# repl-protocol

CLOS protocol for a session with a live Lisp image: eval, output, debugger, inspector, compiler notes, xref, threads. Nick: `stack-repl`. Third pillar of [`lisp-mcp`](https://github.com/egao1980/lisp-mcp).

| Backend | Wire |
|---------|------|
| [`repl-backend-swank`](https://github.com/egao1980/repl-backend-swank) | SLIME Swank |
| [`repl-backend-slynk`](https://github.com/egao1980/repl-backend-slynk) | Sly Slynk |

```lisp
(asdf:load-system "repl-backend-swank")
(asdf:load-system "process-backend-uiop")
(process-backend-uiop:use-uiop-process-backend)

(let ((conn (stack-repl:repl-spawn (repl-backend-swank:make-swank-backend))))
  (unwind-protect
       (let ((state (stack-repl:repl-eval conn "(error \"boom\")")))
         ;; entering the debugger is a returned DEBUGGER-STATE, not a hang
         (stack-repl:repl-backtrace conn :end 5)
         (stack-repl:repl-invoke-restart conn 0))   ; → aborted EVAL-RESULT
    (stack-repl:repl-disconnect conn :kill t)))
```

- **The client never evaluates or interns anything from the image.** The wire reader is a small dedicated parser: symbols become `wire-symbol`s (only `nil`, `t` and keywords that already exist map to local symbols), `#.` is refused, and numbers are parsed only from strict number syntax.
- **The debugger is a value.** `repl-eval` returns an `eval-result` or a `debugger-state`. `repl-invoke-restart` resumes the interrupted request and returns its outcome, which may be another `debugger-state`.
- **Conditions:** `repl-timeout` (restarts `keep-waiting`, `interrupt`), `repl-connection-error`, `repl-spawn-error` (with the image's output), `repl-remote-error`, `repl-protocol-error`.
- **Spawning:** `repl-spawn` runs `*default-image-command*` (`ros run --`) with forms that load the server and write its port file. It needs a `process-protocol` backend.
- **Shared layer:** `swank-family-connection` implements every GF once. A backend supplies an `rpc-symbol` table and spawn forms.
- **Conformance:** `repl-protocol/conformance` is one suite that every backend runs against a spawned image.

## License

MIT
