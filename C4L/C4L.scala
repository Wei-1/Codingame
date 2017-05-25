import math._
import scala.util._

/**
 * Bring data on patient samples from the diagnosis machine to the laboratory with enough molecules to produce medicine!
 **/
object Player extends App {
    
    def pt(str: String) = Console.err.println(str)

    val ELEMENTLIMIT = 10
    val DIAGNOSIS = 1
    val MOLECULES = 2
    val LABORATORY = 3
    val SAMPLES = 4
    val N = -1
    val A = 0
    val B = 1
    val C = 2
    val D = 3
    val E = 4
    val ELEMENTMAX = 5
    val ELEMENTS = Array("A", "B", "C", "D", "E")
    val INITCOST = Array(0, 0, 0, 0, 0)
    val GAINMAP = Map("A" -> 0, "B" -> 1, "C" -> 2, "D" -> 3, "E" -> 4, "0" -> -1)

    class Paper(
        val id: Int = -1,
        val owner: Int = -1,
        val rank: Int = 0,
        val gain: Int = 0,
        val hp: Int = 0,
        val cost: Array[Int] = INITCOST) {
    }

    class Project(
        val cost: Array[Int] = INITCOST) {
        var take: Boolean = false
        def check(expert: Array[Int]): Boolean = {
            if (expert.zip(cost).forall { case (a, b) => a >= b }) {
                take = true
            }
            return take
        }
        def dist(expert: Array[Int]): Int = {
            return expert.zip(cost).map { case (a, b) => Math.max(0, a - b) }.sum
        }
        def value(expert: Array[Int], gain: Int): Int = {
            val tmp = expert.zip(cost).map { case (a, b) => Math.max(0, a - b) }
            if (tmp(gain) > 0) {
                return 50 / tmp.sum
            } else {
                return 0
            }
        }
    }
    
    class Player(
        val id: Int = -1,
        val eta: Int = -1,
        val score: Int = -1,
        val have: Array[Int] = INITCOST,
        val expert: Array[Int] = INITCOST) {

        def can(ocp: Array[Int]): Array[Int] = have.zip(expert).zip(ocp).map { case ((h, e), o) =>
            val v = h + e - o
            if (v >= 0) Math.max(e, v)
            else -100
        }
        def need(c: Array[Int], ocp: Array[Int] = INITCOST): Array[Int] = {
            if (c.exists(_ < 0)) return Array(100, 100, 100, 100, 100)
            else return c.zip(can(ocp)).map { case (a, b) => Math.max(0, a - b) }
        }
        def still: Int = ELEMENTLIMIT - have.sum
        def papervalue(paper: Paper): Int = {
            val tmp = need(paper.cost)
            if (paper.hp < 0) return -100
            else return paper.hp - tmp.sum - (Math.pow(tmp.max-1, 2).toInt - 10) - (expert(paper.gain) - 3) // Required Max Element Weighting
        }
        def papersvalue(papers: Array[Paper], projects: Array[Project]): Array[(Paper, Int)] = {
            return papers.map { paper =>
                if (paper.hp < 0) (paper, -100)
                else (paper, papervalue(paper) +
                    papers.map { tmpaper =>
                        if (tmpaper.id == paper.id) 0
                        else {
                            val tmp = need(tmpaper.cost)
                            if (tmp(paper.gain) > 0) tmpaper.hp / 5
                            else 0
                        }
                    }.sum + projects.filter(!_.take).map(_.value(expert, paper.gain)).foldLeft(0)(_ + _))
            }
        }
        def solve(papers: Array[Paper], projects: Array[Project]): (Array[Paper], Array[Paper], Array[Int]) = {
            var arr = Array[(Paper, Int)]()
            var yarr = Array[Paper]()
            var narr = Array[Paper]()
            val pvs = papersvalue(papers, projects).sortBy(-_._2)
            val ocp = INITCOST.clone

            pt("____")
            pt("h: " + have.mkString(","))
            pt("e: " + expert.mkString(","))
            for ((paper, value) <- pvs) {
                val needsum = need(paper.cost, ocp).sum
                if (needsum <= 0) {
                    for (i <- 0 until 5) ocp(i) += paper.cost(i)
                    yarr :+= paper
                } else {
                    narr :+= paper
                }
                pt("p"+ paper.id + ": " + paper.cost.mkString(",") + " " + value + " " + needsum + " " + ocp.mkString(","))
            }
            pt("y: " + yarr.size + " n: " + narr.size)
            pt("____")
            
            return (yarr, narr, ocp)
        }
        def expertsum: Int = expert.sum
        def newclone: Player = new Player(id, eta, score, have, expert)
        def spend(cost: Array[Int]): Array[Int] = expert.zip(cost).map(l => Math.max(0, l._2 - l._1))
        def takeclone(paper: Paper): Player = {
            if (need(paper.cost).sum <= 0)
                return new Player(id, eta, score + paper.hp,
                    have.zip(spend(paper.cost)).map(l => l._1 - l._2), {
                        val tmp = expert.clone
                        tmp(paper.gain) += 1
                        tmp
                    })
            else return null
        }
        def useless(papers: Array[Paper], projects: Array[Project]): Array[Int] = {
            val (yarr, narr, ocp) = solve(papers, projects)
            val left = have.zip(spend(ocp)).map(l => l._1 - l._2)
            narr.map { paper =>
                val tmp = need(paper.cost)
                for (i <- 0 until 5) {
                    left(i) = Math.max(0, left(i) - tmp(i))
                }
            }
            return left
        }
    }

    class Samples() {
        def findSample(player: Player, papers: Array[Paper]): Int = {
            if (papers.size >= 3) {
                return -1
            } else if (player.expert.min >= 2) {
                return 3
            } else {
                return Math.min(2, Math.max(1, (player.expertsum - papers.map(_.rank).sum) / (3 - papers.size)))
            }
        }
    }

    class Diagnosis() {
        def findDiagnose(player: Player, papers: Array[Paper]): Int = {
            val uarr = papers.filter { paper =>
                paper.hp < 0
            }
            if (uarr.size == 0) {
                return -1
            } else {
                return uarr.head.id
            }
        }
        def findOrder(player: Player, mypapers: Array[Paper], otpapers: Array[Paper], projects: Array[Project]): Int = {
            val myids = mypapers.map(paper => paper.id)
            val (yarr, narr, ocp) = player.solve(mypapers ++ otpapers, projects)
            if (yarr.size >= 3) {
                if (mypapers.size < 3) {
                    for (paper <- yarr.take(3)) {
                        if (!myids.contains(paper.id)) {
                            return paper.id
                        }
                    }
                } else {
                    val tids = yarr.take(3).map(paper => paper.id)
                    var tmparr = Array[Paper]()
                    for (paper <- mypapers) {
                        if (!tids.contains(paper.id)) {
                            tmparr :+= paper
                        }
                    }
                    if (tmparr.size > 0) return tmparr.maxBy(paper => player.need(paper.cost).sum).id
                }
            } else if (yarr.size > 0) {
                if (mypapers.size < 3) {
                    for (paper <- yarr) {
                        if (!myids.contains(paper.id)) {
                            return paper.id
                        }
                    }
                } else {
                    val tids = yarr.map(paper => paper.id)
                    if (!tids.forall(id => myids.contains(id))) {
                        var tmparr = Array[Paper]()
                        for (paper <- mypapers) {
                            if (!tids.contains(paper.id)) {
                                tmparr :+= paper
                            }
                        }
                        if (tmparr.size > 0) return tmparr.maxBy(paper => player.need(paper.cost).sum).id
                    }
                }
            }
            return -1
        }
    }

    class Molecules() {
        var have: Array[Int] = INITCOST

        def load(newhave: Array[Int]) = have = newhave

        // TODO return IND and Score -> Select Block or Peak
        // Score -> Blockable vs Un-Blockable
        def findMolecule(player: Player, papers: Array[Paper], projects: Array[Project]): Int = {
            if (player.still <= 0) {
                return -1
            } else {
                val tmp = papers.map(paper => player.need(paper.cost).map(v => if (v > 0) 1 else 0))
                val tmpcost = INITCOST.clone
                for (i <- 0 until tmp.size) {
                    for (j <- 0 until 5)
                        tmpcost(j) += tmp(i)(j)
                }
                val (yarr, narr, ocp) = player.solve(papers, projects)
                var check = true
                var willtake = -1
                var i = 0
                while(check && i < narr.size) {
                    var incheck = true
                    val cansolveneed = player.need(narr(i).cost, ocp)
                    willtake = have.zip(cansolveneed).zip(tmpcost).map { case ((h, c), t) =>
                        if (h < c) incheck = false
                        if (h > 0 && c > 0) {
                            ELEMENTMAX - h + c + t
                        } else if (h > 0) 0 else -1
                    }.zipWithIndex.maxBy(_._1)._2
                    if (incheck && cansolveneed.sum <= player.still) {
                        check = false
                    } else {
                        willtake = -1
                    }
                    i += 1
                }
                return willtake
            }
        }
        def findScore(oyarr: Array[Paper], onarr: Array[Paper], eyarr: Array[Paper], enarr: Array[Paper]): Int = {
            val oscore = oyarr.map(_.hp).sum - onarr.map(_.hp).sum
            val ocount = oyarr.size - onarr.size
            val escore = eyarr.map(_.hp).sum - enarr.map(_.hp).sum
            val ecount = eyarr.size - enarr.size
            return oscore - escore + (ocount - ecount) * 20
        }
        def findEnough(player: Player, papers: Array[Paper]): (Array[Paper], Array[Paper]) = {
            val tmpresult = papers.partition { paper =>
                val tmpcost = player.need(paper.cost)
                tmpcost.sum <= player.still &&
                tmpcost.zip(have).forall { case (c, h) => h >= c }
            }
            pt("________")
            pt("Enough: " + player.id)
            pt("y: " + tmpresult._1.size + " n: " + tmpresult._2.size)
            pt("________")
            return tmpresult
        }
        def newclone: Molecules = {
            val tmp = new Molecules
            tmp.load(have.clone)
            return tmp
        }
    }

    class Laboratory() {
        def findMake(player: Player, papers: Array[Paper], projects: Array[Project]): Int = {
            val (yarr, narr, ocp) = player.solve(papers, projects)
            if (yarr.size > 0) {
                return yarr.head.id
            } else {
                return -1
            }
        }
    }
    
    val projectcount = readInt
    val projects = new Array[Project](projectcount)
    for(i <- 0 until projectcount) {
        val inputline = readLine
        pt(inputline)
        projects(i) = new Project(inputline split " " map(_.toInt))
    }

    val molecules = new Molecules
    val samples = new Samples
    val diagnosis = new Diagnosis
    val laboratory = new Laboratory

    var lastdrop = false

    // game loop
    while(true) {
        var oat: Int = 0
        var oplayer: Player = null
        var eat: Int = 0
        var eplayer: Player = null
        var mypapers = Array[Paper]()
        var otpapers = Array[Paper]()
        var enpapers = Array[Paper]()
        
        for(i <- 0 until 2) {
            val inputline = readLine
            pt(inputline)
            val Array(target, _eta, _score, _storagea, _storageb, _storagec, _storaged, _storagee,
                _expertisea, _expertiseb, _expertisec, _expertised, _expertisee) = inputline split " "
            val eta = _eta.toInt
            val score = _score.toInt
            val storagea = _storagea.toInt
            val storageb = _storageb.toInt
            val storagec = _storagec.toInt
            val storaged = _storaged.toInt
            val storagee = _storagee.toInt
            val expertisea = _expertisea.toInt
            val expertiseb = _expertiseb.toInt
            val expertisec = _expertisec.toInt
            val expertised = _expertised.toInt
            val expertisee = _expertisee.toInt
            val storage = Array(storagea, storageb, storagec, storaged, storagee)
            val expert = Array(expertisea, expertiseb, expertisec, expertised, expertisee)
            projects.map(project => project.check(expert))
            if (i == 0) {
                oplayer = new Player(i, eta, score, storage, expert)
                oat = target match {
                    case "DIAGNOSIS" => 1
                    case "MOLECULES" => 2
                    case "LABORATORY" => 3
                    case "SAMPLES" => 4
                    case _ => 0
                }
            } else if (i == 1) {
                eplayer = new Player(i, eta, score, storage, expert)
                eat = target match {
                    case "DIAGNOSIS" => 1
                    case "MOLECULES" => 2
                    case "LABORATORY" => 3
                    case "SAMPLES" => 4
                    case _ => 0
                }
            }
        }

        val inputline = readLine
        pt(inputline)
        molecules.load(inputline split " " map(_.toInt))

        var dropping = false

        val samplecount = readInt
        for(i <- 0 until samplecount) {
            val inputline = readLine
            pt(inputline)
            val Array(_sampleid, _carriedby, _rank, expertisegain, _health, _costa, _costb, _costc, _costd, _coste) = inputline split " "
            val sampleid = _sampleid.toInt
            val carriedby = _carriedby.toInt
            val rank = _rank.toInt
            val gain = GAINMAP(expertisegain)
            val health = _health.toInt
            val costa = _costa.toInt
            val costb = _costb.toInt
            val costc = _costc.toInt
            val costd = _costd.toInt
            val coste = _coste.toInt
            if (carriedby == 0) {
                mypapers :+= new Paper(sampleid, carriedby, rank, gain, health, Array(costa, costb, costc, costd, coste))
            } else if (carriedby == -1) {
                otpapers :+= new Paper(sampleid, carriedby, rank, gain, health, Array(costa, costb, costc, costd, coste))
            } else if (carriedby == 1) {
                enpapers :+= new Paper(sampleid, carriedby, rank, gain, health, Array(costa, costb, costc, costd, coste))
            }
        }

        val myprojects = projects.filter(project =>
            !project.take &&
            project.dist(oplayer.expert) <= project.dist(eplayer.expert)
        )
        val enprojects = projects.filter(project =>
            !project.take &&
            project.dist(oplayer.expert) >= project.dist(eplayer.expert)
        )

        if (oplayer.eta == 0) {
            println(oat match {
                case SAMPLES => {
                    val ind = samples.findSample(oplayer, mypapers)
                    if (ind < 0) {
                        "GOTO DIAGNOSIS"
                    } else {
                        "CONNECT " + ind
                    }
                }
                case DIAGNOSIS => {
                    val ind = diagnosis.findDiagnose(oplayer, mypapers)
                    if (ind < 0) {
                        val oind = diagnosis.findOrder(oplayer, mypapers, otpapers, projects)
                        if (oind < 0) {
                            if (mypapers.size >= 3) {
                                val (yarr, narr, ocp) = oplayer.solve(mypapers, myprojects)
                                if (yarr.size > 0) {
                                    "GOTO LABORATORY"
                                } else {
                                    val (oyarr, onarr) = molecules.findEnough(oplayer, mypapers)
                                    if (oyarr.size > 0) "GOTO MOLECULES"
                                    else {
                                        dropping = true
                                        if (onarr.size > 0) "CONNECT " + onarr.maxBy(paper => oplayer.need(paper.cost).sum).id
                                        else "CONNECT " + mypapers.maxBy(_.cost.max).id
                                    }
                                }
                            } else {
                                val (oyarr, onarr) = molecules.findEnough(oplayer, mypapers)
                                val (tyarr, tnarr) = molecules.findEnough(oplayer, otpapers)
                                val (eyarr, enarr) = molecules.findEnough(eplayer, otpapers)
                                if (tyarr.size > 0) {
                                    "CONNECT " + oplayer.papersvalue(tyarr, myprojects).maxBy(_._2)._1.id
                                } else if (mypapers.size == 0) {
                                    "GOTO SAMPLES"
                                } else if (mypapers.size == 1 && eyarr.size > 0) {
                                    "CONNECT " + oplayer.papersvalue(eyarr, myprojects).maxBy(_._2)._1.id
                                } else if (oyarr.size > 0) {
                                    "GOTO MOLECULES"
                                } else if (onarr.size > 0 && lastdrop) {
                                    dropping = true
                                    "CONNECT " + onarr.maxBy(paper => oplayer.need(paper.cost).sum).id
                                } else if (mypapers.size < 3) {
                                    "GOTO SAMPLES"
                                } else if (oplayer.have.sum < 10) {
                                    "GOTO MOLECULES"
                                } else {
                                    "GOTO DIAGNOSIS"
                                }
                            }
                        } else {
                            "CONNECT " + oind
                        }
                    } else {
                        "CONNECT " + ind
                    }
                }
                case MOLECULES => {
                    val oind = molecules.findMolecule(oplayer, mypapers, myprojects)
                    val eind = molecules.findMolecule(eplayer, enpapers, enprojects)
                    // pt("ELEMENT " + ind)
                    pt("ELEMENT " + oind)
                    if (oind < 0) {
                        if (oplayer.still > 0 && eind >= 0 && oplayer.useless(mypapers, projects).sum < 8) {
                            "CONNECT " + ELEMENTS(eind)
                        } else {
                            val (yarr, narr, ocp) = oplayer.solve(mypapers, myprojects)
                            if (yarr.size > 0) {
                                "GOTO LABORATORY"
                            } else {
                                val (yarr, narr) = molecules.findEnough(oplayer, otpapers)
                                if (yarr.size == 0 && mypapers.size < 3) {
                                    "GOTO SAMPLES"
                                } else {
                                    "GOTO DIAGNOSIS"
                                }
                            }
                        }
                    } else {
                        "CONNECT " + ELEMENTS(oind)
                    }
                }
                case LABORATORY => {
                    val ind = laboratory.findMake(oplayer, mypapers, myprojects)
                    if (ind < 0) {
                        val (yarr, narr) = molecules.findEnough(oplayer, mypapers)
                        if (yarr.size > 0) {
                            "GOTO MOLECULES"
                        } else {
                            val (yarr, narr) = molecules.findEnough(oplayer, otpapers)
                            if (yarr.size == 0 && mypapers.size < 3) {
                                "GOTO SAMPLES"
                            } else {
                                "GOTO DIAGNOSIS"
                            }
                        }
                    } else {
                        "CONNECT " + ind
                    }
                }
                case _ => {
                    "GOTO SAMPLES"
                }
            })
        } else println("Wei prints 18 chars")

        lastdrop = dropping
        // Write an action using println
        // To debug: Console.err.println("Debug messages...")
    }
}