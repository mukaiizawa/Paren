xxxxxxxxxx0Error -- "eq? already bound".
	at: (unwind-protect (let (in (.open-read $G-464)) (<- $G-463 in) (labels :start (if (not (neq? (eval (read in)) :EOF)) (goto :break)) (begin) :continue (goto :start) :break nil)) (if $G-463 (.close $G-463)))
	at: (let ($G-463 nil) (unwind-protect (let (in (.open-read $G-464)) (<- $G-463 in) (labels :start (if (not (neq? (eval (read in)) :EOF)) (goto :break)) (begin) :continue (goto :start) :break nil)) (if $G-463 (.close $G-463))))
	at: (let ($G-464 (if (string? path) (.init (.new Path) path) path)) (if (not (is-a? $G-464 Path)) ((lambda (:rest args) (throw (.message (.new Error) (apply string args)))) $G-464 " is not a path")) (let ($G-463 nil) (unwind-protect (let (in (.open-read $G...
	at: (load script)
	at: (let (script (car $args) args (cdr $args)) (<- $args args) (load script) (if (bound? 'main) (main)))
	at: (if (nil? $args) (repl) (let (script (car $args) args (cdr $args)) (<- $args args) (load script) (if (bound? 'main) (main))))
	at: (boot)
