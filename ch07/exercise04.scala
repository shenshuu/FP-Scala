type Par[A] = ExecutorService => Future[A]

def fork[A](a : => Par[A]) : Par[A] =
    es => es.submit(new Callable[A]) {
        def call = a(es).get
    }

def unit(a : A) : Par[A] =
    es => a(es)

def lazyUnit(a : A) : Par[A] =
    fork(unit(a))

def asyncF[A,B](f : A => B) : A => Par[B] =
    a => lazyUnit(f(a))