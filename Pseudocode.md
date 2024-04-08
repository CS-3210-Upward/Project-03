(defun function-call (function parameters)
    ;; Create a new frame on the call stack for the function
    (let ((new-frame (create-new-frame)))
        ;; Bind parameters to local variables in the new frame
        (dolist (parameter parameters)
            (bind-variable (parameter-name parameter) (parameter-value parameter) new-frame))
        ;; Execute the function's code within the new frame
        (let ((result (execute-function function new-frame)))
            ;; Remove the frame from the call stack
            (remove-frame new-frame)
            ;; Return the result of the function
            result)))

(defun create-new-frame ()
    ;; Allocate memory for a new frame
    (allocate-memory))

(defun remove-frame (frame)
    ;; Deallocate memory for the frame
    (deallocate-memory frame))
