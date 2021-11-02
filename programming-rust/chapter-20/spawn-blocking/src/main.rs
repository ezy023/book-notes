use std::sync::{Arc, Mutex};
use std::task::Waker;
use waker_fn::waker_fn;
use futures_lit::pin;
use crossbeam::sync::Parker;
use std::future::Future;
use std::pin::Pin;
use std::task::{Context, Poll}:

pub struct SpawnBlocking<T>(Arc<Mutex<Shared<T>>>);

// The Shared struct serves as a rendezvous between the future and the thread running the closure
// Polling the future checks whether 'value' is present and saves the waker in 'waker' if not
// The thread running the closure saves its return value in 'value' and then invokes 'waker', if present.
struct Shared<T> {
    value: Option<T>,
    waker: Option<Waker>,
}

pub fn spawn_blocking<T,F>(closure: F) -> SpawnBlocking<T>
where F: FnOnce() -> T,
      F: Send + 'static,
      T: Send + 'static,
{
    let inner = Arc::new(Mutex::new(Shared {
        value: None,
        waker: None,
    }));

    std::thread::spawn({
        let inner = inner.clone();
        move || {
            let value = closure();                     // run the closure to get the value

            let maybe_waker = {
                let mut guard = inner.lock().unwrap(); // unlock the mutex
                guard.value = Some(value);             // set the 'value'
                guard.waker.take()                     // move the 'waker' into 'maybe_waker'
            };

            if let Some(waker) = maybe_waker {         // if there is a waker, consume the waker with '.wake()'
                waker.wake();
            }
        }
    });

    SpawnBlocking(inner)
}


impl<T: Send> Future for SpawnBlocking<T> {
    type Output = T;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<T> {
        let mut guard = self.0.lock().unwrap();
        if let Some(value) = guard.value.take() {
            return Poll::Ready(value);
        }

        guard.waker = Some(cx.waker().clone());
        Poll::Pending
    }
}


fn block_con<F: Future>(future: F) -> F::Output   {
    let parker = Parker::new();                        // Parker is a blocking primitive that blocks the thread until someone else calls '.unpark()' on the corresponding Unparker
    let unparker = parker.unparker().clone();          // unparker will unblock the parked thread
    let waker = waker_fn(move || unparker.unpark());
    let mut context = Context::from_waker(&waker);

    pin!(future);                                      // 'pin!' macro takes ownership of the future and declares a new variable fo the same name whose type is 'Pin<&mut F>' and that borrows the future.
                                                       // This type is needed by the poll method

    loop {
        match future.as_mut().poll(&mut context)  {
            Poll::Ready(value) => return value,
            Poll::Pending => parker.park(),
        }
    }
}
