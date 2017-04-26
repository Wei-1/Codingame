import math._
import scala.util._

/**
 * Auto-generated code below aims at helping you parse
 * the standard input according to the problem statement.
 **/
object Player extends App {

    val PREDICTION = 4
    val WAIT = 0
    val FASTER = 1
    val SLOWER = 2
    val PORT = 3
    val STARBOARD = 4
    val movestr = Array("WAIT", "FASTER", "SLOWER", "PORT", "STARBOARD")
    val fastarr = Array(WAIT, SLOWER, PORT, STARBOARD)
    val slowarr = Array(WAIT, SLOWER, FASTER, PORT, STARBOARD)
    val stoparr = Array(WAIT, FASTER, PORT, STARBOARD)
    val movearr = Array(stoparr, slowarr, fastarr)

    var ships = Map[Int, Ship]()
    var barrels = Map[Int, Barrel]()
    var cannons = Map[Int, Cannon]()
    var mines = Map[Int, Mine]()
    var collidemap = Map[(Int, Int), Array[Int]]()
    var barrelmap = Map[(Int, Int), Int]()
    var cannonmap = Map[(Int, Int), Array[Cannon]]()
    var mineset = Set[(Int, Int)]()

    def mod(x: Int, y: Int): Int = {
        val v = x % y
        if (v < 0) return v + y
        else return v
    }

    def abc2xy(a: Int, b: Int, c: Int): (Int, Int) = {
        val x = a + (c - (c & 1)) / 2
        val y = c
        return (x, y)
    }

    def xy2abc(x: Int, y: Int): (Int, Int, Int) = {
        val a = x - (y - (y & 1)) / 2
        val c = y
        val b = - a - c
        return (a, b, c)
    }

    def r2abc(r: Int): (Int, Int, Int) = r match {
        case 0 => (1, -1, 0)
        case 1 => (1, 0, -1)
        case 2 => (0, 1, -1)
        case 3 => (-1, 1, 0)
        case 4 => (-1, 0, 1)
        case 5 => (0, -1, 1)
    }

    def r2v(r: Int): Vector = {
        val (a, b, c) = r2abc(r)
        return new Vector(a, b, c)
    }

    def dist(x1: Int, y1: Int, x2: Int, y2: Int): Int = {
        val (a1, b1, c1) = xy2abc(x1, y1)
        val (a2, b2, c2) = xy2abc(x2, y2)
        return ((a1 - a2).abs + (b1 - b2).abs + (c1 - c2).abs) / 2
    }

    def nextRotate(rotate: Int, act: Int): Int = {
        return mod(rotate + (if (act == PORT) 1 else if (act == STARBOARD) -1 else 0), 6)
    }

    def nextLoc(x: Int, y: Int, rotate: Int, speed: Int, act: Int):
        (Int, Int, Int, Int) = {
        val nspeed = speed + (if (act == FASTER) 1 else if (act == SLOWER) -1 else 0)
        val (nx, ny) = r2v(rotate).pvt(x, y, nspeed)
        val nrotate = nextRotate(rotate, act)
        return (nx, ny, nrotate, nspeed)
    }

    def ingrid(x: Int, y: Int): Boolean = {
        return x >= 0 && x < 23 && y >= 0 && y < 21
    }

    def valid(x: Int, y: Int, rotate: Int): Boolean = {
        val v = r2v(rotate)
        val (hx, hy) = v.pvt(x, y, 1)
        val (tx, ty) = v.pvt(x, y, -1)
        return ingrid(x, y) && ingrid(hx, hy) && ingrid(tx, ty)
    }

    def getPoint(t: Int, x: Int, y: Int, rotate: Int, act: Int, id: Int): Int = {
        val v = r2v(rotate)
        val (hx, hy) = v.pvt(x, y, 1)
        val (tx, ty) = v.pvt(x, y, -1)
        if (ingrid(x, y) && ingrid(hx, hy) && ingrid(tx, ty)) {
            var score = 0
            if (hx <= 1 || hx >= 21) score -= 10
            if (hy <= 1 || hy >= 19) score -= 10
            val collidearr = collidemap.getOrElse((hx, hy), Array[Int]())
            if (collidearr.exists(_ != id)) score -= 20
            if (mineset.contains((hx, hy))) score -= 25
            if (mineset.contains((x, y))) score -= 25
            if (mineset.contains((tx, ty))) score -= 25
            score += barrelmap.getOrElse((hx, hy), 0)
            score += barrelmap.getOrElse((x, y), 0)
            score += barrelmap.getOrElse((tx, ty), 0)
            val cannonh = cannonmap.getOrElse((hx, hy), null)
            val cannono = cannonmap.getOrElse((x, y), null)
            val cannont = cannonmap.getOrElse((tx, ty), null)
            if (cannonh != null) score -= cannonh.filter(_.count == t).size * 25
            if (cannono != null) score -= cannono.filter(_.count == t).size * 50
            if (cannont != null) score -= cannont.filter(_.count == t).size * 25
            if (act == PORT) {
                v.loadrotate(nextRotate(rotate, STARBOARD))
                val (lx, ly) = v.pvt(x, y, 1)
                val collidearr = collidemap.getOrElse((lx, ly), Array[Int]())
                if (collidearr.exists(_ != id)) score -= 20
                if (mineset.contains((lx, ly))) score -= 25
                score += barrelmap.getOrElse((lx, ly), 0)
                val cannonl = cannonmap.getOrElse((lx, ly), null)
                if (cannonl != null) score -= cannonl.filter(_.count == t).size * 25
            } else if (act == STARBOARD) {
                v.loadrotate(nextRotate(rotate, PORT))
                val (lx, ly) = v.pvt(x, y, 1)
                val collidearr = collidemap.getOrElse((lx, ly), Array[Int]())
                if (collidearr.exists(_ != id)) score -= 20
                if (mineset.contains((lx, ly))) score -= 25
                score += barrelmap.getOrElse((lx, ly), 0)
                val cannonl = cannonmap.getOrElse((lx, ly), null)
                if (cannonl != null) score -= cannonl.filter(_.count == t).size * 25
            }
            return score
        } else return Int.MinValue
    }

    class Vector(var av: Int = 0, var bv: Int = 0, var cv: Int = 0) {
        def loadp2p(x1: Int, y1: Int, x2: Int, y2: Int) {
            val (a1, b1, c1) = xy2abc(x1, y1)
            val (a2, b2, c2) = xy2abc(x2, y2)
            av = a2 - a1
            bv = b2 - b1
            cv = c2 - c1
        }
        def loadv(a: Int, b: Int, c: Int) {
            av = a
            bv = b
            cv = c
        }
        def loadrotate(r: Int) {
            val (a, b, c) = r2abc(r)
            loadv(a, b, c)
        }
        def addv(a: Int, b: Int, c: Int) {
            av += a
            bv += b
            cv += c
        }
        def addv(v: Vector) {
            av += v.av
            bv += v.bv
            cv += v.cv
        }
        def pvt(x: Int, y: Int, t: Int): (Int, Int) = {
            var (a, b, c) = xy2abc(x, y)
            a += av * t
            b += bv * t
            c += cv * t
            return abc2xy(a, b, c)
        }
        def diffv(v: Vector): Int = {
            val ad = (av - v.av).abs
            val bd = (bv - v.bv).abs
            val cd = (cv - v.cv).abs
            return ad + bd + cd
        }
        def unit = {
            val (maxi, mini) = if (av > bv) {
                val xi = if (av > cv) 0 else 2
                val ni = if (bv < cv) 1 else 2
                (xi, ni)
            } else {
                val xi = if (bv > cv) 1 else 2
                val ni = if (av < cv) 0 else 2
                (xi, ni)
            }
            val (a, b, c) = {
                if (maxi == 0 && mini == 1) (1, -1, 0)
                else if (maxi == 0 && mini == 2) (1, 0, -1)
                else if (maxi == 1 && mini == 0) (-1, 1, 0)
                else if (maxi == 1 && mini == 2) (0, 1, -1)
                else if (maxi == 2 && mini == 0) (-1, 0, 1)
                else (0, -1, 1)
            }
            av = a
            bv = b
            cv = c
        }
        def d: Int = (av.abs + bv.abs + cv.abs) / 2
    }

    class Mine(val id: Int = 0, val x: Int = -1, val y: Int = -1) {
        def getXY = (x, y)
        def see: Boolean = {
            return ships.exists { case (id, ship) =>
                ship.owner == 1 && ship.d(x, y) <= 5
            }
        }
    }

    class Barrel(val id: Int = 0, val x: Int = -1, val y: Int = -1, val count: Int = -1) {
        def getXY = (x, y)
        def d(ship: Ship): Int = {
            val (tx, ty) = ship.getXY
            return dist(x, y, tx, ty)
        }
        def d(tx: Int, ty: Int): Int = {
            return dist(x, y, tx, ty)
        }
    }

    class Cannon(val id: Int = 0, var x: Int = -1, var y: Int = -1, var count: Int = -1) {
        def getXY = (x, y)
        def load(nx: Int, ny: Int, ncount: Int) {
            x = nx
            y = ny
            count = ncount
        }
    }

    class Ship(val id: Int, var x: Int, var y: Int,
        var rotate: Int, var speed: Int, var count: Int, var owner: Int) {
        var fire: Int = 0
        var mine: Int = 0
        var actions = Array[Int]()
        def load(nx: Int, ny: Int, nrotate: Int, nspeed: Int, ncount: Int) {
            actions = Array[Int]()
            x = nx
            y = ny
            rotate = nrotate
            speed = nspeed
            count = ncount
            if (fire > 0) fire -= 1
            if (mine > 0) mine -= 1
        }
        def shot = fire = 2
        def laid = mine = 5
        def getXY = (x, y)
        def d(ship: Ship): Int = {
            val (tx, ty) = ship.getXY
            return dist(x, y, tx, ty)
        }
        def d(tx: Int, ty: Int): Int = {
            return dist(x, y, tx, ty)
        }
        def head: (Int, Int) = {
            val v = r2v(rotate)
            return v.pvt(x, y, 1)
        }
        def body: (Int, Int) = (x, y)
        def tail: (Int, Int) = {
            val v = r2v(rotate)
            return v.pvt(x, y, -1)
        }
        def potential: Array[(Int, Int)] = {
            val v = r2v(rotate)
            val pv = r2v(nextRotate(rotate, PORT))
            val sv = r2v(nextRotate(rotate, STARBOARD))
            var arr = Array((x, y))
            if (speed == 0) {
                return arr ++ Array(v.pvt(x, y, -1), v.pvt(x, y, 1), v.pvt(x, y, 2),
                    pv.pvt(x, y, -1), pv.pvt(x, y, 1),
                    sv.pvt(x, y, -1), sv.pvt(x, y, 1))
            } else if (speed == 1) {
                val (nx, ny) = v.pvt(x, y, 1)
                return arr ++ Array(v.pvt(nx, ny, -2), (nx, ny), v.pvt(nx, ny, 1), v.pvt(nx, ny, 2),
                    pv.pvt(nx, ny, -1), pv.pvt(nx, ny, 1),
                    sv.pvt(nx, ny, -1), sv.pvt(nx, ny, 1))
            } else if (speed == 2) {
                val (nx, ny) = v.pvt(x, y, 2)
                return arr ++ Array(v.pvt(nx, ny, -1), (nx, ny), v.pvt(nx, ny, 1),
                    pv.pvt(nx, ny, -1), pv.pvt(nx, ny, 1),
                    sv.pvt(nx, ny, -1), sv.pvt(nx, ny, 1))
            } else {
                return Array[(Int, Int)]()
            }
        }
        def predict: Array[Int] = {
            Console.err.println("Id:" + id + " x:" + x + " y:" + y +" Speed:" + speed)
            // point, (x, y, rotate, speed), moves
            var arr = Array((count, (x, y, rotate, speed), Array[Int]()))
            for (i <- 1 to PREDICTION) {
                val tarr = arr
                arr = Array[(Int, (Int, Int, Int, Int), Array[Int])]()
                for ((tp, (tx, ty, tr, ts), ta) <- tarr) {
                    // Console.err.println(i + " " + ts)
                    movearr(ts).map { act =>
                        val (nx, ny, nr, ns) = nextLoc(tx, ty, tr, ts, act)
                        val point = getPoint(i, nx, ny, nr, act, id)
                        if (point + tp > 0 && ns >= 0 && ns < 3) {
                            arr :+= (point + tp + dist(nx, ny, x, y), (nx, ny, nr, ns), ta :+ act)
                        }
                    }
                }
            }
            if (arr.size == 0) {
                Console.err.println(id + " NO PATH")
                return Array(0)
            } else arr.maxBy(_._1)._3
        }
    }

    // game loop
    while(true) {
        collidemap = Map[(Int, Int), Array[Int]]()
        barrelmap = Map[(Int, Int), Int]()
        cannonmap = Map[(Int, Int), Array[Cannon]]()
        mineset = Set[(Int, Int)]()
        var survivor = Array[Int]()
        val myshipcount = readInt // the number of remaining ships
        val entitycount = readInt // the number of entities (e.g. ships, mines or cannonballs)
        for(i <- 0 until entitycount) {
            val inputline = readLine
            // Console.err.println(inputline)
            val Array(_entityid, entitytype, _x, _y, _arg1, _arg2, _arg3, _arg4) = inputline split " "
            val id = _entityid.toInt
            val x = _x.toInt
            val y = _y.toInt
            val arg1 = _arg1.toInt
            val arg2 = _arg2.toInt
            val arg3 = _arg3.toInt
            val arg4 = _arg4.toInt
            survivor :+= id
            if (entitytype == "SHIP") {
                if (!ships.contains(id))
                    ships += id -> new Ship(id, x, y, arg1, arg2, arg3, arg4)
                else
                    ships(id).load(x, y, arg1, arg2, arg3)
            } else if (entitytype == "BARREL") {
                if (!barrels.contains(id)) {
                    barrels += id -> new Barrel(id, x, y, arg1)
                }
            } else if (entitytype == "CANNONBALL") {
                if (!cannons.contains(id))
                    cannons += id -> new Cannon(id, x, y, arg1)
                else
                    cannons(id).load(x, y, arg1)
            } else if (entitytype == "MINE") {
                if (!mines.contains(id)) {
                    Console.err.println("MINE at " + x + " " + y)
                    mines += id -> new Mine(id, x, y)
                }
            }
        }

        ships = ships.filter { case (id, _) => survivor.contains(id) }
        barrels = barrels.filter { case (id, _) => survivor.contains(id) }
        cannons = cannons.filter { case (id, _) => survivor.contains(id) }
        mines = mines.filter { case (id, mine) =>
            survivor.contains(id) || !mine.see
        }

        barrels.map { case (id, barrel) => barrelmap += barrel.getXY -> barrel.count }
        cannons.map { case (id, cannon) =>
            val clist = cannonmap.getOrElse(cannon.getXY, Array[Cannon]())
            cannonmap += cannon.getXY -> (clist :+ cannon)
        }
        mines.map { case (id, mine) => mineset += mine.getXY }
        ships.map { case (id, ship) => collidemap ++= ship.potential.map { k =>
            val sarr = collidemap.getOrElse(k, Array[Int]())
            (k -> (sarr :+ ship.id))
        }}

        var newships = ships
        var actions = Map[Int, String]()
        while (newships.size > 0) {
            val (ship, (bid, d, bx, by)) = newships.map { case (sid, ship) =>
                val (sx, sy) = ship.getXY
                val targetarr = barrels.map { case (bid, barrel) =>
                    val (bx, by) = barrel.getXY
                    (bid, dist(sx, sy, bx, by), bx, by)
                }
                if (targetarr.size > 0) {
                    (ship, targetarr.minBy(_._2))
                } else {
                    (ship, (-1, Int.MaxValue, -1, -1))
                }
            }.minBy(_._2._2)
            newships -= ship.id
            barrels -= bid
            if (ship.owner == 1) {
                val acts = ship.predict
                Console.err.println(acts.mkString(" "))
                val act = acts.head
                if (act == 0 && ship.fire == 0) {
                    val (ox, oy) = ship.getXY
                    val (ex, ey, ed) = ships.filter(_._2.owner != 1).map { case (eid, eship) =>
                        val (ex, ey) = eship.getXY
                        val ed = eship.d(ship)
                        val et = ed / 3 + 1
                        val v = new Vector
                        v.loadrotate(eship.rotate)
                        val (tx, ty) = v.pvt(ex, ey, et * eship.speed)
                        (tx, ty, dist(ox, oy, tx, ty))
                    }.minBy(_._3)
                    if (ed < 10 && ingrid(ex, ey)) {
                        ship.shot
                        val action = "FIRE " + ex + " " + ey
                        actions += ship.id -> action
                    } else if (ship.mine == 0) {
                        ship.laid
                        val action = "MINE"
                        actions += ship.id -> action
                    } else {
                        val action = "WAIT"
                        actions += ship.id -> action
                    }
                } else {
                    actions += ship.id -> movestr(act)
                }
            }
        }
        actions.toArray.sortBy(_._1).map { case (id, action) =>
            println(action)
        }
    }
}