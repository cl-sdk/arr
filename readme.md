# arr[^1]

![arr](https://raw.githubusercontent.com/cl-sdk/arr/refs/heads/main/assets/github-banner.png "arr")

Common lisp background workers, eventually a task queue and task orchestration (coordination).

---

## Usage

> NOTE: Your application must handle process termination signals to stop the application.

### `arr.background-worker`

There are 2 function to start and stop the background worker:

#### Start

```lisp
(arr.background-worker:start-application)
```

#### Stop

```lisp
(arr.background-worker:stop-application)
```

### Custom implementation

All the generics are available in other to build your worker.

#### Task implementation

```lisp
(defgeneric task (kind data &key time &allow-other-keys))
```

#### Scheduler and runner worker function

```lisp
(defgeneric task-runner (app &key &allow-other-keys))
```

```lisp
(defgeneric schedule-task (kind scheduled-time data &key app &allow-other-keys))
```

```lisp
(defgeneric task-scheduler (app &key &allow-other-keys))
```

##### Enqueueing tasks

```lisp
(defgeneric execute-task (app task &optional data))
```

```lisp
(defgeneric execute-task-at (app time task &optional data))
```

---

# License

Unlicense.

See [license](https://github.com/cl-sdk/arr/blob/main/license).

---

[^1]: `arr` is the name of the function of the haskell's `Control.Arrows` package, that lifts a pure function into a computation.
