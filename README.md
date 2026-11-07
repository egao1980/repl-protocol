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
- **Stepping:** `repl-step conn frame :mode :into|:next|:out` resumes the stepper (code compiled with `(debug 3)`, entered with `cl:step`) and returns the next stop or the result.
- **Channels** (Slynk): `repl-open-channel` gives a separate REPL with its own thread, package and value history. `repl-channel-eval` returns an `eval-result` whose `eval-result-entry` names the history entry, so later input can say `#v3:0`; `repl-channel-inspect` inspects an entry. Swank signals `repl-not-supported`.
- **Events:** `(setf repl-event-listener)` gets `(kind data)` from the reader thread for `:output`, `:debug`, `:debug-return`, `:indentation` (Swank/Slynk indentation updates, also kept in `repl-indentation`) and `:channel`. A failing listener becomes a warning, never a dead connection.
- **Conditions:** `repl-timeout` (restarts `keep-waiting`, `interrupt`), `repl-connection-error`, `repl-spawn-error` (with the image's output), `repl-remote-error`, `repl-protocol-error`, `repl-not-supported`.
- **Spawning:** `repl-spawn` runs `*default-image-command*` (`ros run --`) with forms that load the server and write its port file. It needs a `process-protocol` backend. The program is looked up on `PATH` (`find-program`), trying `.exe`/`.cmd`/`.bat` on Windows. When the default `ros` is not found, the image is this Lisp's own runtime and core (SBCL), with this image's Quicklisp loaded.
- **Shared layer:** `swank-family-connection` implements every GF once. A backend supplies an `rpc-symbol` table and spawn forms.
- **Conformance:** `repl-protocol/conformance` is one suite that every backend runs against a spawned image.

## License

MIT
