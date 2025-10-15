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

`task` is a generic definition and can be executed by all workers available.

It is (supposed) a stateless function that is fire and forget.

```lisp
(defgeneric task (kind data &key time &allow-other-keys))
```

#### Scheduler and runner worker function

`task-runner` is the engine of the dispatcher worker. At this moment, all tasks, immediate or schedules, are ready to dispatch.

```lisp
(defgeneric task-runner (app &key &allow-other-keys))
```

Just like `task-runner`, `task-scheduler` is the engine of the scheduling and queueing tasks when they are ready to be dispatched.

```lisp
(defgeneric task-scheduler (app &key &allow-other-keys))
```

`schedule-task` receives the "task plan":

- How it should be scheduled
- Task data

```lisp
(defgeneric schedule-task (kind scheduled-time data &key app &allow-other-keys))
```

As an example, to schedule a immediate or a scheduled (timed) task:

```lisp
(defmethod schedule-task ((kind (eql :immeditate) scheduled-time data &key app &allow-other-keys)
  ;; ...
  )

(defmethod schedule-task ((kind (eql :scheduled-task) scheduled-time data &key app &allow-other-keys)
  ;;...
  )
```

##### Enqueueing tasks

```lisp
(defgeneric execute-task (app task &optional data))
```

```lisp
(defgeneric execute-task-at (app time task &optional data))
```

---

## License

[Unlicense](https://github.com/cl-sdk/arr/blob/main/license)

[^1]: `arr` is the name of the function of the haskell's `Control.Arrows` package, that lifts a pure function into a computation.
