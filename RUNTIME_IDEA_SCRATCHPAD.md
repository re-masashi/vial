# Runtime Ideas Scratchpad

Ideas for runtime features.

## Custom Schedulers

### Scheduler Trait

```rs
trait Scheduler
  type TaskId
  fn spawn<T>(mut self, f: fn() -> T) -> Task<T, Self>
  fn yield(mut self)
end
```

### Runtime Block (Structured Concurrency)

```ruby
let rt = Runtime<WorkStealing>.new(4)

runtime rt do
  spawn work()
  spawn other()
  
  let ch = Channel<int>.new()
  spawn ch.send(42)
  let v = ch.recv()
end  # waits for all spawns to complete
```

**Structured concurrency**: block waits for all spawned tasks before exiting.

**Escape hatch**: explicitly detach if needed:

```ruby
runtime rt do
  let t = spawn work()
  t.detach()  # allow orphan
end
```
