/*package Helpers

/// Nice implementation of generators found on :
//	https://github.com/jimmc/scoroutine/blob/master/src/main/scala/net/jimmc

import scala.collection.Iterator
import scala.util.continuations._
import net.jimmc.scoroutine
/** Generic generator class.
 */
class Generator[T] extends Iterator[T] {
  
    val sched = new DefaultCoScheduler
    val buf = new CoQueue[T](sched,1)

    /** Subclass calls this method to generate values.
     * @param body The code for your generator.
     */
    def generate(body: => Unit) {
        sched.addRoutine("gen") { body }
        sched.run
    }

    /** Yield the next generated value.
     * Call this code from your generator to deliver the next value.
     */
    protected def yld(x:T):Unit = {
        buf.blockingEnqueue(x)
    }

    /** Retrieve the next generated value.

 * Call this from your main code.
     */
    def next:T = {
        sched.run
        buf.dequeue
    }

    /** True if there is another value to retrieve.
     * Call this from your main code.
     */
    def hasNext:Boolean = {
        sched.run
        !buf.dequeueBlocker.isBlocked
    }
}*/
