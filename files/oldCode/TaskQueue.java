package bitnots.eventthreads;

import java.io.*;
import java.util.ArrayList;
import java.util.NoSuchElementException;

/**
 * <p>A queue of Runnables.
 * 
 * <p>The contents of the queue are not serialized.
 * 
 * <p>This queue has a thread associated with it.  This thread
 * continually dequeues and dispatches tasks.
 * 
 * <p>This will need methods to allow the elements to be rearranged.
 * 
 * <p>This provides a way of putting "blocking tasks" anywhere on the
 * queue.  When a blocking task is reached by the dispatch thread, the
 * thread will block until some other thread sets the onHold property
 * of the TaskQueue to false.  If the onHold property is set to false
 * when a blocking task is at the front of the queue, the blocking
 * task will be removed from the queue.
 * 
 * <p>This does not allow two blocking tasks to occur next to each
 * other in the queue under any circumstances.
 * 
 * <p>This is a source of UnexpectedExceptionEvents.  These are thrown
 * when a Runnable on the queue throws an exception other than an
 * InterruptedException.  If a client wants to be notified of this
 * event, it should register an UnexpectedExceptionListener.
 * 
 * @author <a href="mailto:shults@cs.wcu.edu">Benjamin Shults</a>
 * @version 1
 */

public class TaskQueue {

  /**
   * 
   */
  private static final long serialVersionUID = 6792083979909540424L;

  private transient TaskDispatchThread spirit;

  // TODO why am I using an ArrayList?  Why not an EventQueue or at least
  // some linked structure.  A circular array would be better as well.

  // index 0 is the front of the queue and index size-1 is the end.
  private transient ArrayList<Runnable> delegate;

  private static final int DEFAULT_CAPACITY = 10;

  private final Runnable blockingTask = new Runnable() {
    /**
     * First put the queue on hold then wait until the queue is
     * taken off hold by another thread.  */
    public void run() {
      TaskQueue.this.setOnHold(true);
      synchronized (TaskQueue.this) {
        while (TaskQueue.this.isOnHold()) {
          try {
            TaskQueue.this.wait();
          } catch (InterruptedException e) {
            // This is an expected interruption.  When onHold goes to
            // false, this exception is thrown.
            //                return;
          }
        }
      }
    }
  };

  /**
   * See documentation of isOnHold to see the invariants that this
   * maintains.  If it was on hold and is no longer and if there was a
   * blocking task at the front of the queue, then the blocking task
   * is removed from the queue.  This is thread safe.  
   * @param putOnHold <code>true</code> if it is to be put on hold. 
   */
  public void setOnHold(boolean putOnHold) {
    synchronized (this) {
      if (putOnHold != this.onHold) {
        this.onHold = putOnHold;
        if (putOnHold) {
          // if the front of the queue is not a BlockingTask, then put
          // one on there.
          if (this.isEmpty()) {
            this.enqueue(this.blockingTask);
            // now TaskDispatchThread will be waiting in
            // blockingTask.
          } else if (this.getFront() != this.blockingTask) {
            this.delegate.add(0, this.blockingTask);
            // when TaskDispatchThread finishes current task, it will
            // start waiting in blockingTask.
          }
        } else if (this.isEmpty() ||
                   this.getFront() != this.blockingTask) {
          // we are taking the hold off so we want to wake the
          // TaskDispatchThread from its sleeping in the blockingTask.
          // The invariant tells us that it must be there under these
          // circumstances.
          this.spirit.interrupt();
        } else
          // It must be that we are removing the hold before the
          // TaskDispatchThread finishes what it was doing when we put
          // the hold on.  So just take the blockingTask off the
          // queue.
          this.delegate.remove(0);
      }
    }
  }

  private boolean onHold;

  /**
   * If this is true, then the TaskDispatchThread will be in one of
   * the following states:
   * 
   * <ol>
   * 
   *   <li> still dispatching the task that it was running before this
   *   was set to true.  In this case, blockingTask will be in the
   *   front of the queue.  </li>
   * 
   *   <li> dispatching blockingTask.  In this case, blockingTask must
   *   not be on top of the queue.  </li>
   * 
   * </ol>
   * 
   * <p>If this is false, then the TaskDispatchThread ought to be
   * dequeueing and dispatching something other than blockingTask.
   * 
   * <p>When the TaskDispatchThread is dispatching blockingTask, it
   * will wait until isOnHold is set to false.  At that point, it will
   * go back to dequeueing and dispatching tasks.
   * @return whether or not the queue is on hold.
   */
  public boolean isOnHold() {
    synchronized (this) {
      return this.onHold;
    }
  }

  /**
   * Put the queue on hold and try to force the dispatch thread to
   * stop doing real work and get into the blockingTask.  If the
   * thread is not doing real work, then leave it be.  */
  public void interruptDispatcher() {
    synchronized (this) {
      if (this.isOnHold()) {
        if (this.getFront() == this.blockingTask) {
          this.spirit.interrupt();
          // the dispatch thread should soon stop what it's doing and
          // pick the blockingTask up off the queue.
        }
        // all good.
      } else {
        this.setOnHold(true);
        // now the blockingTask will be at the front of the queue and
        // the dispatch thread will either be waiting to get the lock
        // to dequeue it or will still be working on a real task.
        this.spirit.interrupt();
      }
    }
  }

  /**
   * Returns a reference to the front of the queue.
   * @return the Runnable that has been on the queue the longest (mod 
   * intentional movements)
   * @throws NoSuchElementException if the queue is empty.
   */
  public Runnable getFront() {
    synchronized (this) {
      if (this.isEmpty())
        throw new NoSuchElementException();
      else
        return this.delegate.get(0);
    }
  }

  /**
   * Return <code>true</code> if there are no Runnables on the queue.
   * @return <code>true</code> if there are no Runnables on the queue.
   */
  public boolean isEmpty() {
    synchronized (this) {
      return this.delegate.isEmpty();
    }
  }

  /**
   * Return the number of Runnables in the queue.
   * @return the number of Runnables in the queue.
   */
  public int size() {
    synchronized (this) {
      return this.delegate.size();
    }
  }

  /**
   * This waits until the queue is nonempty and then removes and
   * returns the element that's been on it the longest or the element
   * that has been moved to the front intentionally.  This should be
   * the only method called by the dispatch thread.
   * @return the Runnable that has been on the queue the longest or
   * that has been moved to the front intentionally.  
   * @throws InterruptedException if the thread is interrupted.
   */
  public Runnable dequeue() throws InterruptedException {
    synchronized (this) {
      while (this.isEmpty())
        this.wait();
      return this.delegate.remove(0);
    }
  }

  /**
   * Adds r to the end of the queue.  This is thread safe.  This
   * ensures that it does not put two blocking tasks next to each
   * other.
   * @param r the Runnable to be added.  */
  public void enqueue(Runnable r) {
    synchronized (this) {
      if (this.delegate.isEmpty()) {
        this.delegate.add(0, r);
        this.notifyAll();
      } else if (r != this.blockingTask ||
                 this.delegate.get(this.size() - 1) != this.blockingTask)
        // if r is not the blockingTask or if it is the blockingTask and
        // the end of the queue is not already the blockingTask, then
        // add r to the queue.
        this.delegate.add(r);
    }
  }

  /**
   * Remove the element of the queue at <b>index</b>.  If <b>index</b>
   * is zero and the front of the queue is a blocking task, then the
   * onHold status of the queue is set to false.  This is thread safe.
   * This does not permit the resulting queue to contain consecutive
   * blocking tasks.
   * @param index 
   * @return the removed Runnable
   * @exception IndexOutOfBoundsException if the index is out of bounds.
   */
  // This will be used by the GUI.
  public Runnable remove(int index) {
    synchronized (this) {
      if (0 == index) {
        if (this.getFront() == this.blockingTask)
          this.setOnHold(false);
        return this.delegate.remove(0);
      } else {
        Runnable r = this.delegate.remove(index);
        if (this.delegate.get(index - 1) == this.blockingTask)
          while (index < this.size() &&
                 this.delegate.get(index) == this.blockingTask)
            this.delegate.remove(index);
        return r;
      }
    }
  }

  /**
   * Moves the element at index <b>from</b> to index <b>to</b>.  This
   * will not allow you to move things to the front of the queue if
   * the blockingTask is currently at the front.  
   * @param from the index of the Runnable to be moved.
   * @param to the index to which it will be moved.
   */
  // This will be used by the GUI to move rearrange the queue.
  public void move(int from, int to) {
    // TODO implement
    synchronized (this) {//
    }
  }

  private ArrayList listeners = new ArrayList(2);

  /**
   * 
   * @param uel
   */
  public void addUnexpectedExceptionListener(
    UnexpectedExceptionListener uel) {
    if (!this.listeners.contains(uel))
      this.listeners.add(uel);
    
  }

  public void removeUnexpectedExceptionListener(
    UnexpectedExceptionListener uel) {
    this.listeners.remove(uel);
  }

  void fireUnexpectedExceptionEvent(Object source, Exception e) {
    UnexpectedExceptionEvent uee =
      new UnexpectedExceptionEvent(source, e);
    ArrayList v; // v will contain a copy of the listener list.

    // Synchronize so that changes cannot occur during cloning.
    synchronized (this) {
      v = (ArrayList) this.listeners.clone();
    }
    // Send the event object to each registered listener.
    for (int i = 0; i < v.size(); i++) {
      // Throw exceptions away.  They are bugs in the listener since
      // none of them are checked exceptions.
      try {
        ((UnexpectedExceptionListener) v.get(i)).unexpectedEvent(uee);
      } catch (Exception ex) {
        //
      }
    }
  }

//  private void readObject(ObjectInputStream ois) throws IOException,
//    ClassNotFoundException {
//    ois.defaultReadObject();
//    this.delegate = new ArrayList<Runnable>(DEFAULT_CAPACITY);
//    this.spirit = new TaskDispatchThread();
//    this.spirit.start();
//  }

  /**
   * Creates a TaskQueue with an initial capacity of ten.
   */
  public TaskQueue() {
    this(DEFAULT_CAPACITY);
  }
  
  /**
   * Creates a TaskQueue with an initial capacity of
   * <b>initialCapacity</b>.  This creates and starts a
   * TaskDispatchThread that will continually dequeue and dispatch
   * tasks from the front of the queue. 
   * @param initialCapacity the initial capacity of the queue. 
   */
  public TaskQueue(int initialCapacity) {
    this.delegate = new ArrayList<Runnable>(initialCapacity);
    this.spirit = new TaskDispatchThread();
    this.spirit.start();
  }
  
  private class TaskDispatchThread extends Thread {

    {
      this.setDaemon(true);
    }
    /**
     * @see java.lang.Thread#run()
     */
    @Override
    public void run() {
      Runnable current = null;
      while (true) {
        try {
          // TODO: clear the interrupted status before running the next
          // task.  I'm not sure this is right but I don't want to think
          // about it right now.
          Thread.interrupted();
          (current = TaskQueue.this.dequeue()).run();
        } catch (InterruptedException e) {
          // expected... just go back to the queue.
        } catch (Exception e) {
          System.err.println("During task dispatching: ");
          // report exceptions to System.err and go back to the queue.
          e.printStackTrace();
          // I want this to notify listeners, also.
          TaskQueue.this.fireUnexpectedExceptionEvent(current, e);
        }
      }
    }

  }

}// TaskQueue

