package bitnots.eventthreads;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

/**
 * 
 * @author bshults
 *
 */
public class SingleThreadExecutorService implements ExecutorService {
  
  // INVARIANT: it is an error if this is negative.
  private int counter = 0;
  
  private final ThreadPoolExecutor e;
  
  /**
   * Returns true if the thread is busy.
   * @return true if the thread is busy.
   */
  public boolean isBusy() {
    return this.counter != 0;
  }
  
  /**
   * @see java.util.concurrent.Executor#execute(java.lang.Runnable)
   */
  public void execute(Runnable command) {
    this.e.execute(new TrackingRunnable(command));
  }
  /**
   * @see java.util.concurrent.ExecutorService#shutdown()
   */
  public void shutdown() {
    this.e.shutdown();
  }
  /**
   * @see java.util.concurrent.ExecutorService#shutdownNow()
   */
  public List<Runnable> shutdownNow() {
    return this.e.shutdownNow();
  }
  /**
   * @see java.util.concurrent.ExecutorService#isShutdown()
   */
  public boolean isShutdown() {
    return this.e.isShutdown();
  }
  /**
   * @see java.util.concurrent.ExecutorService#isTerminated()
   */
  public boolean isTerminated() {
    return this.e.isTerminated();
  }
  /**
   * @see java.util.concurrent.ExecutorService#awaitTermination(long, java.util.concurrent.TimeUnit)
   */
  public boolean awaitTermination(final long timeout, final TimeUnit unit)
  throws InterruptedException {
    return this.e.awaitTermination(timeout, unit);
  }
  /**
   * @see java.util.concurrent.ExecutorService#submit(java.lang.Runnable)
   */
  public Future<?> submit(final Runnable task) {
    return this.e.submit(new TrackingRunnable(task));
  }
  /**
   * @see java.util.concurrent.ExecutorService#submit(java.util.concurrent.Callable)
   */
  public <T> Future<T> submit(Callable<T> task) {
    return this.e.submit(new TrackingCallable<T>(task));
  }
  /**
   * @param task 
   * @param result 
   * @param <T> 
   * @return @see java.util.concurrent.ExecutorService#submit(java.lang.Runnable, java.lang.Object)
   */
  public <T> Future<T> submit(Runnable task, T result) {
    return this.e.submit(new TrackingRunnable(task), result);
  }
  /**
   * @see java.util.concurrent.ExecutorService#invokeAll(java.util.Collection)
   */
  public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks)
  throws InterruptedException {
    ArrayList<TrackingCallable<T>> newTasks =
      new ArrayList<TrackingCallable<T>>(tasks.size());
    for (Callable<T> callable: tasks) {
      newTasks.add(new TrackingCallable<T>(callable));
    }
    return this.e.invokeAll(newTasks);
  }
  /**
   * @see java.util.concurrent.ExecutorService#invokeAll(java.util.Collection, long, java.util.concurrent.TimeUnit)
   */
  public <T> List<Future<T>> invokeAll(Collection<? extends Callable<T>> tasks, 
                                                  long timeout, TimeUnit unit) 
                                                  throws InterruptedException {
    ArrayList<TrackingCallable<T>> newTasks =
      new ArrayList<TrackingCallable<T>>(tasks.size());
    for (Callable<T> callable: tasks) {
      newTasks.add(new TrackingCallable<T>(callable));
    }
    return this.e.invokeAll(newTasks, timeout, unit);
  }
  /**
   * @see java.util.concurrent.ExecutorService#invokeAny(java.util.Collection)
   */
  public <T> T invokeAny(Collection<? extends Callable<T>> tasks)
  throws InterruptedException, ExecutionException {
    ArrayList<TrackingCallable<T>> newTasks =
      new ArrayList<TrackingCallable<T>>(tasks.size());
    for (Callable<T> callable: tasks) {
      newTasks.add(new TrackingCallable<T>(callable));
    }
    return this.e.invokeAny(newTasks);
  }
  /**
   * @see java.util.concurrent.ExecutorService#invokeAny(java.util.Collection, long, java.util.concurrent.TimeUnit)
   */
  public <T> T invokeAny(Collection<? extends Callable<T>> tasks, 
                                    long timeout, TimeUnit unit) 
  throws InterruptedException, ExecutionException, TimeoutException {
    ArrayList<TrackingCallable<T>> newTasks =
      new ArrayList<TrackingCallable<T>>(tasks.size());
    for (Callable<T> callable: tasks) {
      newTasks.add(new TrackingCallable<T>(callable));
    }
    return this.e.invokeAny(newTasks, timeout, unit);
  }
  
  private class TrackingRunnable implements Runnable {
    private Runnable runnable;
    public TrackingRunnable(Runnable r) {
      SingleThreadExecutorService.this.counter++;
      this.runnable = r;
    }
    public void run() {
      try {
        this.runnable.run();
      } finally {
        SingleThreadExecutorService.this.counter--;
      }
    }
  }
  
  private class TrackingCallable<T> implements Callable<T> {
    private Callable<T> callable;
    public TrackingCallable(Callable<T> r) {
      SingleThreadExecutorService.this.counter++;
      this.callable = r;
    }
    public T call() throws Exception {
      T value;
      try {
        value = this.callable.call();
      } finally {
        SingleThreadExecutorService.this.counter--;
      }
      return value;
    }
  }
  
  /**
   * 
   */
  public SingleThreadExecutorService() {
    this.e =
      new ThreadPoolExecutor(1, 1, 0L, TimeUnit.MILLISECONDS, 
                             new LinkedBlockingQueue<Runnable>(),
                             new ThreadFactory() {
        @Override
        public Thread newThread(Runnable r) {
          Thread value = new Thread(r);
          value.setDaemon(true);
          return value;
        }});/* {
      public void afterExecute(Runnable r, Throwable t) {
        super.afterExecute(r, t);
        if (t != null)
          if (t instanceof Error)
            throw (Error) t;
      }
    };*/
  }
}
