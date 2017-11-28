import Math._ // WEI
// ---------------- CLASS ---------------- //
trait Splash {
    val id: Int
    val x: Double
    val y: Double
    val radius: Double
    var extra: Int
    var extra2: Int
    def dist(s: Splash): Double = sqrt(pow(x - s.x, 2) + pow(y - s.y, 2))
    def dist(v: Vehicle): Double = v.dist(this)
    override def toString = s"""$id: $x,$y $radius,$extra"""
}
class Wreck(val id: Int, val x: Double, val y: Double, val radius: Double, var extra: Int, var extra2: Int) extends Splash {
    var oil: Oil = null
    def calScore(w: Wreck): Double = { // IMPORTANT SCORING FUNCTION with MAGIC
        val division = max(1, if(w.oil == null) 1 else w.oil.extra)
        val sqrtdist = sqrt(dist(w))
        (100 - sqrtdist) / division + w.extra * 100 / max(1, sqrtdist)
    }
    def calScore(t: Tanker): Double = (100 - sqrt(t.dist(this))) / 1.5
}
class Tar(val id: Int, val x: Double, val y: Double, val radius: Double, var extra: Int, var extra2: Int) extends Splash { }
class Oil(val id: Int, val x: Double, val y: Double, val radius: Double, var extra: Int, var extra2: Int) extends Splash { }

class User(val id: Int, var score: Int, var rage: Int) {
    override def toString = s"""$id: $score, $rage"""
}

trait Vehicle {
    val id: Int
    val ownid: Int
    var x: Double
    var y: Double
    var vx: Double
    var vy: Double
    val mass: Double
    val radius: Double
    val friction: Double
    val a: Double = 300.0 / mass
    val dt: Double = sqrt((10 - 1 / friction) * a / friction) / 10
    def caldist(tx: Double, ty: Double, tvx: Double, tvy: Double): Double = 
        sqrt(pow(x + vx - tx - tvx, 2) + pow(y + vy - ty - tvy, 2))
    def calrealdist(tx: Double, ty: Double) = sqrt(pow(x - tx, 2) + pow(y - ty, 2))
    def dist(v: Vehicle): Double = caldist(v.x, v.y, v.vx, v.vy)
    def dist(s: Splash): Double = caldist(s.x, s.y, 0, 0)
    def realdist(v: Vehicle): Double = calrealdist(v.x, v.y)
    def realdist(s: Splash): Double = calrealdist(s.x, s.y)
    def caltime(d: Double): Double = friction * d / a + (1 - pow(1 - friction, sqrt(d) / dt)) / friction
    def time(v: Vehicle): Double = caltime(dist(v))
    def time(s: Splash): Double = caltime(dist(s))
    override def toString: String = s"""$id - $ownid: $x,$y $vx,$vy $mass,$radius $friction,$dt"""
}
class Reaper(val id: Int, val ownid: Int, var x: Double, var y: Double, var vx: Double, var vy: Double,
    val mass: Double = 0.5, val radius: Double = 400, val user: User = null, val friction: Double = 0.2) extends Vehicle { }
class Destroyer(val id: Int, val ownid: Int, var x: Double, var y: Double, var vx: Double, var vy: Double,
    val mass: Double = 1.5, val radius: Double = 400, val user: User = null, val friction: Double = 0.3) extends Vehicle { }
class Doof(val id: Int, val ownid: Int, var x: Double, var y: Double, var vx: Double, var vy: Double,
    val mass: Double = 1, val radius: Double = 400, val user: User = null, val friction: Double = 0.25) extends Vehicle { }
class Tanker(val id: Int, val ownid: Int, var x: Double, var y: Double, var vx: Double, var vy: Double,
    val mass: Double = 2.5, val radius: Double = 400,
    var water: Int = 1, var capacity: Int = 1, val friction: Double = 0.4) extends Vehicle { }

// PATH STRATEGY
class Path(x1: Double, y1: Double, x2: Double, y2: Double) {
    def dist(x3: Double, y3: Double, x4: Double, y4: Double) = sqrt(pow(x3 - x4, 2) + pow(y3 - y4, 2))
    val minx = min(x1, x2)
    val maxx = max(x1, x2)
    val miny = min(y1, y2)
    val maxy = max(y1, y2)
    val dx = x2 - x1
    val dy = y2 - y1
    val x2y1_y2x1 = x2 * y1 - y2 * x1
    val d = dist(x1, y1, x2, y2)
    def within(x: Double, y: Double, r: Double): Boolean = {
        if(d > 0 && x > minx && x < maxx && y > miny && y < maxy) {
            r > abs(dy * x - dx * y + x2y1_y2x1) / d
        } else {
            r > min(dist(x1, y1, x, y), dist(x2, y2, x, y))
        }
    }
    def within(v: Vehicle, r: Double): Boolean = within(v.x, v.y, r)
    def within(s: Splash, r: Double): Boolean = within(s.x, s.y, r)
}

// ---------------- OBJECT ---------------- //
object Player extends App {
    def outMove(x: Double, y: Double, p: Double): Unit = {
        if(sqrt(pow(x, 2) + pow(y, 2)) >= 6000) println("WAIT")
        else println(x.toInt + " " + y.toInt + " " + max(0, min(300, p.toInt)))
    }
    def outSkill(x: Double, y: Double): Unit = {
        if(sqrt(pow(x, 2) + pow(y, 2)) >= 6000) println("WAIT")
        else println("SKILL " + x.toInt + " " + y.toInt)
    }
    // game loop
    while(true) {
        // -------- INITIAL -------- //
        val users = Seq(0, 1, 2).map(i => (i, readInt)).map(kv => kv._1 -> new User(kv._1, kv._2, readInt)).toMap

        var myreaper: Reaper = null
        var otreapers = Array[Reaper]()
        var mydestroyer: Destroyer = null
        var otdestroyers = Array[Destroyer]()
        var mydoof: Doof = null
        var otdoofs = Array[Doof]()
        var tankers = Array[Tanker]()
        var wrecks = Array[Wreck]()
        var tars = Array[Tar]()
        var oils = Array[Oil]()
        
        val unitcount = readInt
        for(i <- 0 until unitcount) {
            val Array(_unitid, _unittype, _player, _mass, _radius, _x, _y, _vx, _vy, _extra, _extra2) = readLine split " "
            val (unitid, unittype, player) = (_unitid.toInt, _unittype.toInt, _player.toInt)
            val (mass, radius) = (_mass.toDouble, _radius.toInt)
            val (x, y, vx, vy) = (_x.toInt, _y.toInt, _vx.toInt, _vy.toInt)
            val (extra, extra2) = (_extra.toInt, _extra2.toInt)
            val user = users.getOrElse(player, null: User)
            unittype match {
                case 0 => {
                    if(player > 0) otreapers :+= new Reaper(unitid, player, x, y, vx, vy, mass, radius, user)
                    else myreaper = new Reaper(unitid, player, x, y, vx, vy, mass, radius, user)
                }
                case 1 => {
                    if(player > 0) otdestroyers :+= new Destroyer(unitid, player, x, y, vx, vy, mass, radius, user)
                    else mydestroyer = new Destroyer(unitid, player, x, y, vx, vy, mass, radius, user)
                }
                case 2 => {
                    if(player > 0) otdoofs :+= new Doof(unitid, player, x, y, vx, vy, mass, radius, user)
                    else mydoof = new Doof(unitid, player, x, y, vx, vy, mass, radius, user)
                }
                case 3 => tankers :+= new Tanker(unitid, player, x, y, vx, vy, mass, radius)
                case 4 => wrecks :+= new Wreck(unitid, x, y, radius, extra, 1)
                case 5 => tars :+= new Tar(unitid, x, y, radius, extra, 1)
                case 6 => oils :+= new Oil(unitid, x, y, radius, extra, 1)
                case _ => {}
            }
        }

        var intersecs = Array[Wreck]()
        for(w1 <- wrecks; w2 <- wrecks; if w1.id < w2.id) {
            val d = w1.dist(w2)
            if(d < w1.radius + w2.radius - 10) {
                val ratio = w1.radius / (w1.radius + w2.radius)
                val (nx, ny) = ((w2.x - w1.x) * ratio + w1.x, (w2.y - w1.y) * ratio + w1.y)
                val (nradius, nextra) = ((w1.radius + w2.radius - d) / 2 - 2, min(w1.extra, w2.extra))
                intersecs :+= new Wreck(-1, nx, ny, nradius, nextra, 2)
            }
        }

        var nextwrecks = Array[Wreck]() // last day try
        for(tanker <- tankers) {
            val destroyer = (otdestroyers :+ mydestroyer).minBy(_.dist(tanker))
            if(destroyer.dist(tanker) < tanker.radius + destroyer.radius - 100) {
                nextwrecks :+= new Wreck(-1, tanker.x, tanker.y, tanker.radius, tanker.water, 1)
            }
        }

        var leftwrecks = Array[Wreck]() // 1.3.
        for(w <- wrecks ++ intersecs ++ nextwrecks) {
            val o = if(oils.size > 0) oils.minBy(_.dist(w)) else null: Oil
            val d = if(oils.size > 0) o.dist(w) else 10000
            if(d >= 1000 + w.radius) {
                leftwrecks :+= w
            } else if(d > 1000 - w.radius + 10) {
                val nradius = (w.radius - (1000 - d)) / 2
                val ratio = (d + w.radius - nradius) / d
                val (nx, ny) = ((w.x - o.x) * ratio + o.x, (w.y - o.y) * ratio + o.y)
                leftwrecks :+= new Wreck(-1, nx, ny, nradius, w.extra, w.extra2)
            } else {
                w.oil = o
                leftwrecks :+= w
            }
        }

        // -------- FIRST REAPER STRATEGY -------- // All 119
        val allWreckScore = leftwrecks.map { wreck =>
            val oilscore = (if(wreck.oil != null) wreck.oil.extra else 0) * 2000 // 1.3. -> 6000 ~ 0
            val wreckscore = if(wrecks.size > 0) wrecks.map(w => wreck.calScore(w)).sum else 0 // 1.1. -> 100 ~ 1500
            val tankerscore = if(tankers.size > 0) tankers.map(t => wreck.calScore(t)).sum else 0 // 1.2. -> 0 ~ 300
            val tankerocuscore = if(tankers.filter(_.dist(wreck) < wreck.radius - 400).size > 0) 2000 else 0
            val radiuscore = sqrt(wreck.radius) * 40 // 1.4. -> 40 ~ 1200
            val waterscore = wreck.extra * 600 * wreck.extra2 // 1.5. -> 600 ~ 6000 ~ 12000 // All 95
            (wreck, wreckscore + radiuscore + waterscore - oilscore - tankerscore - tankerocuscore)
        }
        val otblocker = (otdestroyers ++ otdoofs)
        val (rx, ry, rp, rc) = if(allWreckScore.size > 0) { // 182 Silver -> 25 Silver
            val targetdoing = allWreckScore.map { case (wreck, score) =>
                val d = myreaper.dist(wreck)
                val reald = myreaper.realdist(wreck)
                val rtime = myreaper.time(wreck)
                val enreaper = otreapers.minBy(r => r.dist(wreck))
                val enreaperdist = enreaper.dist(wreck)
                val enreaperrealdist = otreapers.map(r => r.realdist(wreck))
                val enblockerdist = otblocker.map(d => d.dist(wreck))
                val enblockerrealdist = otblocker.map(d => d.realdist(wreck))
                val path = new Path(myreaper.x, myreaper.y, wreck.x, wreck.y)

                val enblockerscore = enblockerrealdist.count(_ < reald) * 300
                val enblockertimescore = otblocker.map(d => d.time(wreck)).count(_ < rtime) * 300
                val inwreckscore = if(reald < wreck.radius) 500 else 0
                val eninwreckscore = if(enreaperrealdist.min < wreck.radius) 200 else 0
                val blockinwreckscore = if(enblockerrealdist.min < wreck.radius) 1000 else 0
                val fastenoughscore = if(inwreckscore == 0 && (eninwreckscore > 0 || enreaperdist < d)) 0 else 400
                val faster2blockscore = if(inwreckscore > 0 && eninwreckscore == 0 && enreaperdist > d) 400 else 0
                val otpathscore = otblocker.count(v => path.within(v, 400)) * 800
                val nupathscore = tankers.count(v => path.within(v, 400)) * 400 // better now
                val mypathscore = Seq(mydestroyer, mydoof).count(v => path.within(v, 400)) * 200
                val tarpathscore = if(d > 3000 && inwreckscore == 0) tars.map(t => if(path.within(t, 500)) 200 * t.extra else 0).sum else 0
                val toolongscore = if(d > 3000 && inwreckscore == 0 && eninwreckscore > 0 && rtime - 1 > wreck.extra) 2000 else 0
                // if we wake up tomorrow and got worse than 100th, delete toolongscore !!!!!!!!
                val nscore = score - d + inwreckscore - eninwreckscore + fastenoughscore + faster2blockscore -
                    enblockerscore - enblockertimescore - otpathscore - mypathscore - nupathscore - // tarpathscore -
                    blockinwreckscore // - toolongscore
                // Console.err.println(wreck.id + ":" + nscore + "  " + score)
                val (tx, ty, tp, tc) = if(inwreckscore > 0 && eninwreckscore == 0 && enreaperdist < wreck.radius) {
                    (enreaper.x - myreaper.vx, enreaper.y - myreaper.vy, 300.0, false)
                } else if(inwreckscore > 0 && d < wreck.radius && myreaper.user.rage >= 120 &&
                    enreaperdist > 1200 && enblockerdist.forall(_ > 1200) && otblocker.exists(_.dist(myreaper) < 1980)) {
                    val blocker = otblocker.minBy(_.dist(myreaper))
                    (blocker.x + blocker.vx * 1.2, blocker.y + blocker.vy * 1.2, 0.0, true)
                } else {
                    (wreck.x - myreaper.vx, wreck.y - myreaper.vy, d, false)
                }
                (wreck, nscore, tx, ty, tp, tc)
            }
            val (_, _, tx, ty, tp, tc) = targetdoing.maxBy(_._2)
            (tx, ty, tp, tc)
        } else if(tankers.filter(_.calrealdist(0, 0) < 5500).size > 0) {
            val tanker = tankers.filter(_.calrealdist(0, 0) < 5500).maxBy { tanker =>
                val waterscore = tanker.water * 500 // 500 ~ 5000
                val wreckscore = if(wrecks.size > 0) {
                    val nearwreck = wrecks.minBy(_.dist(tanker))
                    val wreckdist = nearwreck.dist(tanker)
                    if(wreckdist < nearwreck.radius) 1000 else 0 // 0 ~ 1000
                } else 0
                val distscore = sqrt(pow(tanker.x, 2) + pow(tanker.y, 2)) / 10 // 0 ~ 600
                val mydistscore = tanker.dist(myreaper) / 10 // 0 ~ 600
                waterscore + wreckscore - distscore - mydistscore
            }
            (tanker.x + tanker.vx, tanker.y + tanker.vy, 300.0, false)
        } else (0.0, 0.0, 100.0, false)
        if(rc) outSkill(rx, ry)
        else outMove(rx, ry, rp)

        // -------- FIRST DESTROYER STRATEGY -------- //
        val enreapertarget = otreapers.map { reaper =>
            if(allWreckScore.size > 0) {
                val (tw, ts) = allWreckScore.maxBy { case (wreck, score) =>
                    score - reaper.dist(wreck)
                }
                (reaper, tw, ts, reaper.dist(tw), reaper.time(tw),  // 1 ~ 5
                    mydestroyer.dist(reaper), mydestroyer.time(tw), // 6 ~ 7
                    mydoof.dist(reaper), mydoof.time(tw),           // 8 ~ 9
                    myreaper.dist(reaper), myreaper.time(tw))       // 10 ~ 11
            } else {
                (reaper, null: Wreck, 0.0, 0.0, 0.0,
                    mydestroyer.dist(reaper), 100000.0, mydoof.dist(reaper), 100000.0, myreaper.dist(reaper), 100000.0)
            }
        }

        val (dx, dy, dc) = if(enreapertarget.map(_._6).min < 800) { // direct fight back
            val enreaper = enreapertarget.minBy(_._6)._1
            (enreaper.x, enreaper.y, false)
        } else if(myreaper.dist(mydestroyer) < 1000) { // don't block
            (mydestroyer.x * 2 - myreaper.x, mydestroyer.y * 2 - myreaper.y, false)
        } else if(enreapertarget.filter(kv => kv._5 > kv._7).size > 0) { // get ahead
            val tw = enreapertarget.filter(kv => kv._5 > kv._7).maxBy(_._3)._2
            (tw.x, tw.y, false)
        } else if(mydestroyer.user.rage >= 90 && enreapertarget.filter(kv => // All 118
            kv._2 != null && kv._4 < 800 && kv._6 < 1980 && kv._10 > 1025).size > 0) {
            val (er, tw, ts, erwd, erwt, erdd, _, _, _, _, _) = enreapertarget.filter(kv => kv._4 < 800 &&
                kv._6 < 1980 && kv._10 > 1025).maxBy(_._3)
            if(erwd > 20) {
                ((er.x + tw.x) / 2, (er.y + tw.y) / 2, true)
            } else {
                val derx = er.x - mydestroyer.x
                val dery = er.y - mydestroyer.y
                (er.x - derx * 10 / erdd, er.y - dery * 10 / erdd, true)
            }
        } else {
            if(tankers.filter(_.calrealdist(0, 0) < 5500).size > 0) {
                val nt = tankers.filter(_.calrealdist(0, 0) < 5500).minBy(t => mydestroyer.dist(t))
                val td = nt.dist(mydestroyer)
                val rntd = nt.dist(myreaper)
                val er = otreapers.minBy(r => r.dist(nt))
                val erntd = nt.dist(er)
                val erdd = er.dist(mydestroyer)
                if(td < 2000 && erntd < rntd) {
                    (mydestroyer.x * 2 - nt.x, mydestroyer.y * 2 - nt.y, false)
                } else if(erntd < rntd) {
                    if(erntd > td) {
                        (er.x + er.vx, er.y + er.vy, false)
                    } else {
                        (0.0, 0.0, false)
                    }
                } else {
                    (nt.x + nt.vx, nt.y + nt.vy, false)
                }
            } else {
                val enreaper = otreapers.minBy(_.dist(mydestroyer))
                (enreaper.x, enreaper.y, false)
            }
        }
        if(dc) outSkill(dx, dy)
        else outMove(dx, dy, 300.0)
        
        val (ox, oy, oc) = if(enreapertarget.map(_._8).min < 800) { // direct fight back
            val enreaper = enreapertarget.minBy(_._8)._1
            (enreaper.x, enreaper.y, false)
        } else if(myreaper.dist(mydoof) < 1000) {
            (mydoof.x * 2 - myreaper.x, mydoof.y * 2 - myreaper.y, false)
        } else if(enreapertarget.filter(kv => kv._5 > kv._9).size > 0) {
            val tw = enreapertarget.filter(kv => kv._5 > kv._9).maxBy(_._3)._2
            (tw.x, tw.y, false)
        } else if(mydoof.user.rage >= 30 && enreapertarget.filter(kv => // All 118
            kv._2 != null && kv._2.oil == null && kv._4 < 800 && kv._8 < 1980 && kv._10 > 2000).size > 0) {
            val (er, tw, ts, erwd, erwt, erdd, _, _, _, _, _) = enreapertarget.filter(kv => kv._4 < 800 &&
                kv._2 != null && kv._2.oil == null && kv._8 < 1980 && kv._10 > 2000).maxBy(_._3)
            (tw.x, tw.y, true) // Very Important Bug Fix in the last Submit
        } else {
            val enreaper = otreapers.minBy(_.dist(mydoof))
            (enreaper.x, enreaper.y, false)
        }
        if(oc) outSkill(ox, oy)
        else outMove(ox, oy, 300.0)
    }
}
