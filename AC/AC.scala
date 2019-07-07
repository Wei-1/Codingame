class U(var x: Int, var y: Int, var d: Char, val id: Int = -1){
    override def toString = x + " " + y + " " + d
}
object Player extends App {
    // Func
    def clone(u: U): U = new U(u.x, u.y, u.d, u.id)
    def torus(u: U): (Int, Int) = torus(u.x, u.y)
    def torus(xy: (Int, Int)): (Int, Int) = torus(xy._1, xy._2)
    def torus(x: Int, y: Int): (Int, Int) = ((x + 19) % 19, (y + 10) % 10)
    def see(u: U): Char = see(u.x, u.y)
    def see(xy: (Int, Int)): Char = see(xy._1, xy._2)
    def see(x: Int, y: Int): Char = grid(y)(x)
    def set(u: U): Unit = set(u.x, u.y, u.d)
    def set(x: Int, y: Int, c: Char): Unit = grid(y)(x) = c
    def state(u: U): Int = u.x * 20000 + u.y * 1000 + u.d * 10 + u.id
    def inverse(c: Char): Char = c match {
        case 'U' => 'D'
        case 'R' => 'L'
        case 'D' => 'U'
        case 'L' => 'R'
        case _ => '#'
    }
    def sortd(c1: Char, c2: Char): Int = {
        var i = 0
        var id = 'U'
        def run: Unit = {
            i = (i + 1) % 4
            id = "URDL"(i)
        }
        while(c1 != id) run
        run
        var c = 0
        while(c2 != id) {
            c += 1
            run
        }
        c
    }
    def next(u: U): U = {
        val nu = clone(u)
        val (nx, ny) = next(u.x, u.y, u.d)
        val c = see(nx, ny)
        nu.x = nx
        nu.y = ny
        if(c != '.') nu.d = c
        if(states.contains(state(nu))) nu.d = '#'
        nu
    }
    def next(x: Int, y: Int, d: Char): (Int, Int) = torus(d match {
        case 'U' => (x, y - 1)
        case 'R' => (x + 1, y)
        case 'D' => (x, y + 1)
        case 'L' => (x - 1, y)
        case _ => (x, y)
    })
    def exits(u: U): String = exits(u.x, u.y)
    def exits(x: Int, y: Int): String = {
        var cor = ""
        val u = see(torus(x, y - 1))
        val r = see(torus(x + 1, y))
        val d = see(torus(x, y + 1))
        val l = see(torus(x - 1, y))
        if(!(u == 'D' || u == '#')) cor += 'U'
        if(!(r == 'L' || r == '#')) cor += 'R'
        if(!(d == 'U' || d == '#')) cor += 'D'
        if(!(l == 'R' || l == '#')) cor += 'L'
        cor
    }
    def findNext(u: U): U = {
        val nu = next(u)
        Console.err.println(nu)
        if(nu.d == '#') {
            if(see(u) == '.') {
                val cor = exits(u).sortBy(d => sortd(u.d, d))
                var check = true
                var tmprobot: U = null
                for(d <- cor if check) {
                    u.d = d
                    val nu2 = next(u)
                    if(nu2.d != '#') {
                        check = false
                        val nr = new U(u.x, u.y, d)
                        set(nr)
                        newarrows :+= nr
                        tmprobot = nu2
                    }
                }
                tmprobot
            } else null: U
        } else nu
    }
    // Init
    val grid = for(i <- 0 until 10) yield readLine.toArray
    val robotcount = readInt
    var robots = (for(i <- 0 until robotcount) yield {
        val Array(_x, _y, direction) = readLine split " "
        new U(_x.toInt, _y.toInt, direction.head, i)
    }).toArray
    // Print Init State
    Console.err.println(grid.map(_.mkString("")).mkString("\n"))
    Console.err.println(robots.mkString("\n"))
    // Start Logic
    var newarrows = Array[U]()
    // Crossion == 3
    for(i <- 0 until 3) {
        var sets = Array[U]()
        for(x <- 0 until 19; y <- 0 until 10 if "URDL".contains(see(x, y))) {
            if(see(next(x, y, see(x, y))) == '#') sets :+= new U(x, y, '#')
        }
        sets.foreach { u => set(u) }
    }
    // The Must
    // for(x <- 0 until 19; y <- 0 until 10 if see(x, y) == '.') {
    //     val cor = exits(x, y)
    //     if(cor.size == 1) newarrows :+= new U(x, y, cor.head)
    // }
    // for(ar <- newarrows) set(ar)
    // The Safe
    val visits = Array.fill[Array[Boolean]](10)(new Array[Boolean](19))
    var states = Set[Int]()
    while(robots.size > 0) {
        var tmprobots = Array[U]()
        for(u <- robots) {
            states += state(u)
            val nu = findNext(u)
            Console.err.println(nu)
            if(nu != null) {
                val nnu = next(nu)
                if(nnu.d == '#' && (visits(nu.y)(nu.x) || see(nu) != '.') && !visits(u.y)(u.x) && see(u) == '.') {
                    val cor = exits(u).sortBy(d => sortd(u.d, d))
                    var check = true
                    var tmprobot: U = null
                    for(d <- cor if check) {
                        u.d = d
                        val nu2 = next(u)
                        if(nu2.d != '#' && next(nu2).d != '#') {
                            check = false
                            val nr = new U(u.x, u.y, d)
                            set(nr)
                            newarrows :+= nr
                            tmprobot = nu2
                        }
                    }
                    if(tmprobot == null) {
                        tmprobots :+= nu
                    } else {
                        tmprobots :+= tmprobot
                    }
                } else {
                    tmprobots :+= nu
                }
            }
            visits(u.y)(u.x) = true
        }
        robots = tmprobots
    }
    // Result
    Console.err.println(grid.map(_.mkString("")).mkString("\n"))
    println(newarrows.mkString(" "))
}