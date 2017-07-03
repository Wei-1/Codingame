


    val dirs = Map("N" -> (-1, 0), "NE" -> (-1, 1), "E" -> (0, 1),
        "SE" -> (1, 1), "S" -> (1, 0), "SW" -> (1, -1),
        "W" -> (0, -1), "NW" -> (-1, -1))
    val dirarr = Array((-1, 0), (-1, 1), (0, 1), (1, 1),
        (1, 0), (1, -1), (0, -1), (-1, -1))
    val pushdirs = Map(
        (-1, 0) -> Seq((-1, 1), (-1, 0), (-1, -1)),
        (-1, 1) -> Seq((0, 1), (-1 ,1), (-1, 0)),
        (0, 1) -> Seq((1, 1), (0, 1), (-1, 1)),
        (1, 1) -> Seq((1, 0), (1, 1), (0, 1)),
        (1, 0) -> Seq((1, -1), (1, 0), (1, 1)),
        (1, -1) -> Seq((0, -1), (1, -1), (0, 1)),
        (0, -1) -> Seq((-1, -1), (0, -1), (1, -1)),
        (-1, -1) -> Seq((-1, 0), (-1, -1), (0, -1))
    )

    def canmove(atlevel: Int, g: Map[(Int, Int), Int], mp: Set[(Int, Int)], ep: Set[(Int, Int)], t: (Int, Int)): Boolean = {
        val tolevel = g.getOrElse(t, 4)
        return tolevel < 4 && tolevel - atlevel <= 1 && !mp.contains(t) && !ep.contains(t)
    }

    def canbuild(g: Map[(Int, Int), Int], mp: Set[(Int, Int)], ep: Set[(Int, Int)], f: (Int, Int)): Boolean = {
        val tolevel = g.getOrElse(f, 4)
        return tolevel < 4 && !mp.contains(f) && !ep.contains(f)
    }
    
    def calpoint(l: Int) : Int = l match {
        case 3 => 20
        case 2 => 3
        case 1 => 1
        case 0 => 0
        case _ => -1000
    }

    def arr2map(tg: Array[Array[Int]]): Map[(Int, Int), Int] = {
        var m = Map[(Int, Int), Int]()
        for (i <- 0 until tg.size) {
            for (j <- 0 until tg.head.size) {
                m += (i, j) -> tg(i)(j)
            }
        }
        return m
    }

    def seescore(rset: Array[(Int, Array[(Int, Array[Int], Boolean)])], fs: Boolean, mode: Boolean = false): Double = {
        var score = 0.0
        var movecount = 0
        rset.map { case (atlevel, atarr) =>
            val thissize = atarr.size
            movecount += thissize
            // First Level Scoring
            score += calpoint(atlevel) * 15
            score += Math.min(4, thissize - 1) * 10
            if (mode) Console.err.println("s1:" + score + " al:" + atlevel)
            // Second Level Scoring
            atarr.map { case (dlevel, darr, dpush) =>
                if (fs) {
                    score += calpoint(dlevel) / 3.0
                } else {
                    score += calpoint(dlevel) / 10.0
                }
                if (mode) Console.err.println("  lv: " + dlevel + " " + darr.mkString(","))
                val darrsize = darr.size
                if (darrsize <= 2) {
                    // Check if push-able or corner-able
                    if (dpush) {
                        // Console.err.println("PUSH COUNT")
                        if (darrsize == 0) {
                            score -= 1000
                        } else if (atlevel > dlevel && atlevel < dlevel + 2) {
                            if (darrsize == 1) {
                                score -= 1000
                            } else {
                                score -= 300
                            }
                        } else {
                            score -= (3 - darrsize) * 100
                        }
                    }
                    score -= 10
                }
                if (atlevel - dlevel > 1) {
                    score -= 3
                }
                score += darr.size
            }
            if (mode) Console.err.println("s2:" + score)
        }
        if (movecount == 0) score -= 1000
        return score
    }

    def calscore(g: Map[(Int, Int), Int], fp: Set[(Int, Int)], sp: Set[(Int, Int)], act: (Int, (Int, Int), (Int, Int)),
        mode: Boolean = false, time: Long = System.currentTimeMillis): Double = {
        var pushableloc = Set[(Int, Int)]()
        val frset = fp.map { p =>
            val (py, px) = p
            val atlevel = g.getOrElse(p, 4)
            val atlevels = dirarr.map { d =>
                val (dy, dx) = d
                val pp = (py + dy, px + dx)
                val (ppy, ppx) = pp
                // add push-able
                if (sp.contains(pp)) {
                    // Console.err.println("MIGHT GET PUSH: " + pp)
                    pushdirs(d).map { case (pushy, pushx) =>
                        val pushp = (pushy + ppy, pushx + ppx)
                        pushableloc += pushp
                    }
                }
                val dlevel = g.getOrElse(pp, 4)
                if (dlevel > Math.min(3, atlevel + 1) || fp.contains(pp) || sp.contains(pp)) {
                    (dlevel, null: Array[Int], false)
                } else {
                    val dlevels = dirarr.map { case (ddy, ddx) =>
                        val ppp = (ppy + ddy, ppx + ddx)
                        if (sp.contains(ppp)) 4
                        else g.getOrElse(ppp, 4)
                    }.filter(l => l < Math.min(4, dlevel + 2))
                    (dlevel, dlevels, false)
                }
            }.filter(_._2 != null)
            (atlevel, atlevels)
        }.toArray
        // Console.err.println(pushableloc.mkString(" , "))
        val srset = sp.map { p =>
            val (py, px) = p
            val atlevel = g.getOrElse(p, 4)
            val atlevels = dirarr.map { case (dy, dx) =>
                val pp = (py + dy, px + dx)
                val (ppy, ppx) = pp
                val dlevel = g.getOrElse(pp, 4)
                if (dlevel > Math.min(3, atlevel + 1) || fp.contains(pp) || sp.contains(pp)) {
                    (dlevel, null: Array[Int], false)
                } else {
                    val dlevels = dirarr.map { case (ddy, ddx) =>
                        val ppp = (ppy + ddy, ppx + ddx)
                        if (fp.contains(ppp)) 4
                        else g.getOrElse(ppp, 4)
                    }.filter(l => l < Math.min(4, dlevel + 2))
                    val pushcheck = pushableloc.contains(pp)
                    // if (pushcheck) Console.err.println("ADD PUSH LOC: " + pp)
                    (dlevel, dlevels, pushcheck)
                }
            }.filter(_._2 != null)
            (atlevel, atlevels)
        }.toArray
        var fscore = seescore(frset, true, mode)
        var sscore = seescore(srset, false, mode)

        // g: Map[(Int, Int), Int], fp: Set[(Int, Int)], sp: Set[(Int, Int)]
        // Floodfill Check
        var ng = g.filter(_._2 < 4)
        var count = 0
        var nfparr = fp.map { p =>
            (p, ng.getOrElse(p, 4))
        }.filter(_._2 < 4)
        var nsparr = sp.map { p =>
            (p, ng.getOrElse(p, 4))
        }.filter(_._2 < 4)
        var fpbalancescore = 0
        var spbalancescore = 0
        if (System.currentTimeMillis - time < 10) {
            while (ng.size > 0 && (nfparr.size > 0 || nsparr.size > 0) && count < 10) {
                if (count % 2 == 0) {
                    val tarr = nfparr
                    nfparr = Set[((Int, Int), Int)]()
                    tarr.map { case ((py, px), v) =>
                        dirarr.map { case (ddy, ddx) =>
                            val pp = (py + ddy, px + ddx)
                            val ppv = ng.getOrElse(pp, 4)
                            if (ppv < 4 && ppv < v + 2) {
                                nfparr += ((pp, ppv))
                                ng -= pp
                                fpbalancescore += 1
                            }
                        }
                    }
                } else {
                    val tarr = nsparr
                    nsparr = Set[((Int, Int), Int)]()
                    tarr.map { case ((py, px), v) =>
                        dirarr.map { case (ddy, ddx) =>
                            val pp = (py + ddy, px + ddx)
                            val ppv = ng.getOrElse(pp, 4)
                            if (ppv < 4 && ppv < v + 2) {
                                nsparr += ((pp, ppv))
                                ng -= pp
                                spbalancescore += 1
                            }
                        }
                    }
                }
                count += 1
            }
        }

        // Action Score
        val (t, d, b) = act
        val actscore = if (t == 0) {
            - calpoint(g.getOrElse(d, 4))
        } else if (t == 1) {
            calpoint(g.getOrElse(b, 4))
        } else 0

        if (mode) Console.err.println("FS:" + fscore + " SS:" + sscore + " FP:" + fpbalancescore + " SP:" + spbalancescore + " A:" + actscore)
        return (fscore - sscore) + (fpbalancescore - spbalancescore) * 15 + actscore * 2
    }

    def testscore(arr: Array[(Array[Array[Int]], Set[(Int, Int)], Set[(Int, Int)], (Int, (Int, Int), (Int, Int)))], caseid: Int): String = {
        val testr = arr.map { case (tg, mp, ep, act) =>
            val g = arr2map(tg)
            - calscore(g, ep, mp, act, true)
        }
        testr.mkString(",") + "\n" +
            "TEST CASE " + caseid + " " +
            (if (testr.zipWithIndex.reverse.maxBy(_._1)._2 != 0) "ERROR"
            else "SUCCESS")
    }

    Console.err.println(Array("\n\nTESTS",
        /* What the best move for
        2 3 2
        2 0 2
        2 1 2
        OWN 0 0
        ENE 1 1
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(2, 3, 2),
                        Array(2, 0, 2),
                        Array(2, 2, 2)
                    ),
                    Set((1, 0)),
                    Set((1, 1)),
                    (0, (1, 0), (2, 1))
                ),
                (
                    Array(
                        Array(2, 3, 2),
                        Array(2, 0, 2),
                        Array(3, 1, 2)
                    ),
                    Set((1, 0)),
                    Set((1, 1)),
                    (0, (1, 0), (2, 0))
                ),
                (
                    Array(
                        Array(2, 4, 2),
                        Array(2, 0, 2),
                        Array(2, 1, 2)
                    ),
                    Set((1, 0)),
                    Set((1, 1)),
                    (0, (1, 0), (0, 1))
                ),
                (
                    Array(
                        Array(3, 3, 2),
                        Array(2, 0, 2),
                        Array(2, 1, 2)
                    ),
                    Set((1, 0)),
                    Set((1, 1)),
                    (0, (1, 0), (0, 0))
                ),
                (
                    Array(
                        Array(2, 3, 2),
                        Array(3, 0, 2),
                        Array(2, 1, 2)
                    ),
                    Set((0, 1)),
                    Set((1, 1)),
                    (0, (0, 1), (1, 0))
                ),
                (
                    Array(
                        Array(3, 3, 2),
                        Array(2, 0, 2),
                        Array(2, 1, 2)
                    ),
                    Set((0, 1)),
                    Set((1, 1)),
                    (0, (0, 1), (0, 0))
                ),
                (
                    Array(
                        Array(2, 3, 3),
                        Array(2, 0, 2),
                        Array(2, 1, 2)
                    ),
                    Set((0, 1)),
                    Set((1, 1)),
                    (0, (0, 1), (0, 2))
                ),
                (
                    Array(
                        Array(2, 3, 2),
                        Array(2, 0, 3),
                        Array(2, 1, 2)
                    ),
                    Set((0, 1)),
                    Set((1, 1)),
                    (0, (0, 1), (1, 2))
                )
            ), 1)
        },
        /* What the best move for
        1 1 2
        1 3 4
        1 4 3
        OWN 1 1
        ENE 0 2
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(1, 1, 2),
                        Array(1, 4, 4),
                        Array(1, 4, 3)
                    ),
                    Set((0, 1)),
                    Set((0, 2)),
                    (0, (0, 1), (1, 1))
                ),
                (
                    Array(
                        Array(2, 1, 2),
                        Array(1, 3, 4),
                        Array(1, 4, 3)
                    ),
                    Set((0, 1)),
                    Set((0, 2)),
                    (0, (0, 1), (0, 0))
                ),
                (
                    Array(
                        Array(1, 1, 2),
                        Array(2, 3, 4),
                        Array(1, 4, 3)
                    ),
                    Set((0, 1)),
                    Set((0, 2)),
                    (0, (0, 1), (1, 0))
                ),
                (
                    Array(
                        Array(1, 1, 2),
                        Array(1, 4, 4),
                        Array(1, 4, 3)
                    ),
                    Set((1, 0)),
                    Set((0, 2)),
                    (0, (1, 0), (1, 1))
                ),
                (
                    Array(
                        Array(2, 1, 2),
                        Array(1, 3, 4),
                        Array(1, 4, 3)
                    ),
                    Set((1, 0)),
                    Set((0, 2)),
                    (0, (1, 0), (0, 0))
                ),
                (
                    Array(
                        Array(1, 1, 2),
                        Array(1, 3, 4),
                        Array(2, 4, 3)
                    ),
                    Set((1, 0)),
                    Set((0, 2)),
                    (0, (1, 0), (2, 0))
                ),
                (
                    Array(
                        Array(1, 2, 2),
                        Array(1, 3, 4),
                        Array(1, 4, 3)
                    ),
                    Set((1, 0)),
                    Set((0, 2)),
                    (0, (1, 0), (0, 1))
                ),
                (
                    Array(
                        Array(1, 1, 2),
                        Array(1, 4, 4),
                        Array(1, 4, 3)
                    ),
                    Set((2, 2)),
                    Set((0, 2)),
                    (0, (2, 2), (1, 1))
                )
            ), 2)
        },
        /* What the best move for
        2 1
        2 3
        OWN 0 0
        ENE 0 1
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(3, 1),
                        Array(2, 3)
                    ),
                    Set((1, 0)),
                    Set((0, 1)),
                    (0, (1, 0), (0, 0))
                ),
                (
                    Array(
                        Array(3, 1),
                        Array(2, 3)
                    ),
                    Set((1, 1)),
                    Set((0, 1)),
                    (0, (1, 1), (0, 0))
                ),
                (
                    Array(
                        Array(2, 1),
                        Array(2, 4)
                    ),
                    Set((1, 0)),
                    Set((0, 1)),
                    (0, (1, 0), (1, 1))
                ),
                (
                    Array(
                        Array(2, 1),
                        Array(3, 3)
                    ),
                    Set((1, 1)),
                    Set((0, 1)),
                    (0, (1, 1), (1, 0))
                )
            ), 3)
        },
        /* What the best move for
        2 1
        2 3
        OWN 0 0
        ENE 1 0
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(2, 2),
                        Array(2, 3)
                    ),
                    Set((1, 1)),
                    Set((1, 0)),
                    (0, (1, 1), (0, 1))
                ),
                (
                    Array(
                        Array(3, 1),
                        Array(2, 3)
                    ),
                    Set((1, 1)),
                    Set((1, 0)),
                    (0, (1, 1), (0, 0))
                ),
                (
                    Array(
                        Array(2, 1),
                        Array(2, 4)
                    ),
                    Set((0, 1)),
                    Set((1, 0)),
                    (0, (0, 1), (1, 1))
                ),
                (
                    Array(
                        Array(3, 1),
                        Array(2, 3)
                    ),
                    Set((0, 1)),
                    Set((1, 0)),
                    (0, (0, 1), (0, 0))
                )
            ), 4)
        },
        /* What the best move for
        3 2
        3 3
        OWN 0 0
        ENE 1 0
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(4, 2),
                        Array(3, 3)
                    ),
                    Set((1, 1)),
                    Set((1, 0)),
                    (0, (1, 1), (0, 0))
                ),
                (
                    Array(
                        Array(3, 3),
                        Array(3, 3)
                    ),
                    Set((1, 1)),
                    Set((1, 0)),
                    (0, (1, 1), (0, 1))
                ),
                (
                    Array(
                        Array(4, 2),
                        Array(3, 3)
                    ),
                    Set((0, 1)),
                    Set((1, 0)),
                    (0, (0, 1), (0, 0))
                ),
                (
                    Array(
                        Array(3, 2),
                        Array(3, 4)
                    ),
                    Set((0, 1)),
                    Set((1, 0)),
                    (0, (0, 1), (1, 1))
                )
            ), 5)
        },
        /* What the best move for
        2 1
        3 3
        OWN 0 0
        ENE 1 0
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(2, 2),
                        Array(3, 3)
                    ),
                    Set((1, 1)),
                    Set((1, 0)),
                    (0, (1, 1), (0, 1))
                ),
                (
                    Array(
                        Array(3, 1),
                        Array(3, 3)
                    ),
                    Set((1, 1)),
                    Set((1, 0)),
                    (0, (1, 1), (0, 0))
                ),
                (
                    Array(
                        Array(3, 1),
                        Array(3, 3)
                    ),
                    Set((0, 1)),
                    Set((1, 0)),
                    (0, (0, 1), (0, 0))
                ),
                (
                    Array(
                        Array(2, 1),
                        Array(3, 4)
                    ),
                    Set((0, 1)),
                    Set((1, 0)),
                    (0, (0, 1), (1, 1))
                )
            ), 6)
        },
        /* What the best move for
        2 0
        3 2
        OWN 0 0
        ENE 1 0
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(2, 1),
                        Array(3, 2)
                    ),
                    Set((1, 1)),
                    Set((1, 0)),
                    (0, (1, 1), (0, 1))
                ),
                (
                    Array(
                        Array(3, 0),
                        Array(3, 2)
                    ),
                    Set((1, 1)),
                    Set((1, 0)),
                    (0, (1, 1), (0, 0))
                ),
                (
                    Array(
                        Array(3, 0),
                        Array(3, 2)
                    ),
                    Set((0, 1)),
                    Set((1, 0)),
                    (0, (0, 1), (0, 0))
                ),
                (
                    Array(
                        Array(2, 0),
                        Array(3, 3)
                    ),
                    Set((0, 1)),
                    Set((1, 0)),
                    (0, (0, 1), (1, 0))
                )
            ), 7)
        },
        /* What the best move for
        3 3 3
        3 3 3
        3 3 2
        OWN 0 0
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(3, 3, 3),
                        Array(3, 3, 3),
                        Array(3, 3, 3)
                    ),
                    Set((1, 1)),
                    Set[(Int, Int)](),
                    (0, (1, 1), (2, 2))
                ),
                (
                    Array(
                        Array(4, 3, 3),
                        Array(3, 3, 3),
                        Array(3, 3, 2)
                    ),
                    Set((1, 1)),
                    Set[(Int, Int)](),
                    (0, (1, 1), (0, 0))
                ),
                (
                    Array(
                        Array(3, 4, 3),
                        Array(3, 3, 3),
                        Array(3, 3, 2)
                    ),
                    Set((1, 1)),
                    Set[(Int, Int)](),
                    (0, (1, 1), (0, 1))
                ),
                (
                    Array(
                        Array(3, 3, 3),
                        Array(3, 3, 3),
                        Array(3, 4, 2)
                    ),
                    Set((1, 1)),
                    Set[(Int, Int)](),
                    (0, (1, 1), (2, 1))
                ),
                (
                    Array(
                        Array(3, 3, 3),
                        Array(3, 3, 3),
                        Array(3, 4, 2)
                    ),
                    Set((1, 0)),
                    Set[(Int, Int)](),
                    (0, (1, 0), (2, 1))
                ),
                (
                    Array(
                        Array(3, 3, 3),
                        Array(3, 4, 3),
                        Array(3, 3, 2)
                    ),
                    Set((1, 0)),
                    Set[(Int, Int)](),
                    (0, (1, 0), (1, 1))
                )
            ), 8)
        },
        /* What the best move for
        2 4 4
        2 3 3
        3 4 4
        OWN 0 0
        ENE 1 1
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(2, 4, 4),
                        Array(2, 4, 3),
                        Array(3, 4, 4)
                    ),
                    Set((0, 0)),
                    Set((1, 2)),
                    (1, (1, 1), (1, 2))
                ),
                (
                    Array(
                        Array(3, 4, 4),
                        Array(2, 3, 3),
                        Array(3, 4, 4)
                    ),
                    Set((1, 0)),
                    Set((1, 1)),
                    (0, (1, 0), (0, 0))
                ),
                (
                    Array(
                        Array(2, 4, 4),
                        Array(2, 3, 3),
                        Array(4, 4, 4)
                    ),
                    Set((1, 0)),
                    Set((1, 1)),
                    (0, (1, 0), (2, 0))
                )
            ), 9)
        },
        /* What the best move for
        2 4 4
        2 3 3
        3 4 1
        OWN 0 0
        ENE 1 1
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(2, 4, 4),
                        Array(2, 4, 3),
                        Array(3, 4, 1)
                    ),
                    Set((0, 0)),
                    Set((2, 2)),
                    (1, (1, 1), (2, 2))
                ),
                (
                    Array(
                        Array(2, 4, 4),
                        Array(2, 4, 3),
                        Array(3, 4, 1)
                    ),
                    Set((0, 0)),
                    Set((1, 2)),
                    (1, (1, 1), (1, 2))
                ),
                (
                    Array(
                        Array(3, 4, 4),
                        Array(2, 3, 3),
                        Array(3, 4, 1)
                    ),
                    Set((1, 0)),
                    Set((1, 1)),
                    (0, (1, 0), (0, 0))
                ),
                (
                    Array(
                        Array(2, 4, 4),
                        Array(2, 3, 3),
                        Array(4, 4, 1)
                    ),
                    Set((1, 0)),
                    Set((1, 1)),
                    (0, (1, 0), (2, 0))
                )
            ), 10)
        },
        /* What the best move for
        4 4 4
        2 2 3
        4 4 4
        OWN 0 0
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(4, 4, 4),
                        Array(2, 3, 3),
                        Array(4, 4, 4)
                    ),
                    Set((1, 2)),
                    Set[(Int, Int)](),
                    (0, (1, 2), (1, 1))
                ),
                (
                    Array(
                        Array(4, 4, 4),
                        Array(2, 3, 3),
                        Array(4, 4, 4)
                    ),
                    Set((1, 0)),
                    Set[(Int, Int)](),
                    (0, (1, 0), (1, 1))
                )
            ), 11)
        },
        /* What the best move for
        3 4
        3 0
        OWN 0 0
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(3, 4),
                        Array(3, 1)
                    ),
                    Set((1, 0)),
                    Set[(Int, Int)](),
                    (0, (1, 0), (1, 1))
                ),
                (
                    Array(
                        Array(4, 4),
                        Array(3, 0)
                    ),
                    Set((1, 0)),
                    Set[(Int, Int)](),
                    (0, (1, 0), (0, 0))
                ),
                (
                    Array(
                        Array(4, 4),
                        Array(3, 0)
                    ),
                    Set((1, 1)),
                    Set[(Int, Int)](),
                    (0, (1, 1), (0, 0))
                ),
                (
                    Array(
                        Array(3, 4),
                        Array(4, 0)
                    ),
                    Set((1, 1)),
                    Set[(Int, Int)](),
                    (0, (1, 1), (1, 0))
                )
            ), 12)
        },
        /* What the best move for
        0 2 2
        2 1 0
        0 2 3
        OWN 0 0
        ENE 1 1
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(0, 2, 2),
                        Array(2, 2, 0),
                        Array(0, 2, 3)
                    ),
                    Set((0, 0)),
                    Set((1, 2)),
                    (1, (1, 1), (1, 2))
                ),
                (
                    Array(
                        Array(0, 2, 2),
                        Array(2, 2, 0),
                        Array(0, 2, 3)
                    ),
                    Set((0, 0)),
                    Set((2, 1)),
                    (1, (1, 1), (2, 1))
                ),
                (
                    Array(
                        Array(0, 2, 2),
                        Array(2, 2, 0),
                        Array(0, 2, 3)
                    ),
                    Set((0, 0)),
                    Set((2, 2)),
                    (1, (1, 1), (2, 2))
                )
            ), 13)
        },
        /* What the best move for
        3 2 3
        4 3 4
        0 4 3
        OWN 0 1
        ENE 1 1
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(3, 2, 3),
                        Array(4, 4, 4),
                        Array(0, 4, 3)
                    ),
                    Set((0, 1)),
                    Set((2, 0)),
                    (1, (1, 1), (2, 0))
                ),
                (
                    Array(
                        Array(3, 3, 3),
                        Array(4, 3, 4),
                        Array(0, 4, 3)
                    ),
                    Set((0, 0)),
                    Set((1, 1)),
                    (0, (0, 0), (0, 1))
                ),
                (
                    Array(
                        Array(3, 3, 3),
                        Array(4, 3, 4),
                        Array(0, 4, 3)
                    ),
                    Set((0, 2)),
                    Set((1, 1)),
                    (0, (0, 2), (0, 1))
                )
            ), 14)
        },
        /* What the better move for
        0 1 0 0
        0 0 0 0
        0 0 0 0
        0 0 1 0
        OWN 0 0
        ENE 3 3
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(1, 1, 0, 0),
                        Array(0, 0, 0, 0),
                        Array(0, 0, 0, 0),
                        Array(0, 0, 1, 0)
                    ),
                    Set((1, 1)),
                    Set((3, 3)),
                    (0, (1, 1), (0, 0))
                ),
                (
                    Array(
                        Array(0, 2, 0, 0),
                        Array(0, 0, 0, 0),
                        Array(0, 0, 0, 0),
                        Array(0, 0, 1, 0)
                    ),
                    Set((1, 1)),
                    Set((3, 3)),
                    (0, (1, 1), (0, 1))
                )
            ), 15)
        },
        /* What the better move for
        2 4 0
        2 3 4
        2 4 0
        OWN 0 0
        ENE 2 0
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(2, 4, 0),
                        Array(2, 4, 4),
                        Array(2, 4, 0)
                    ),
                    Set((1, 0)),
                    Set((2, 0)),
                    (0, (1, 0), (1, 1))
                ),
                (
                    Array(
                        Array(3, 4, 0),
                        Array(2, 3, 4),
                        Array(2, 4, 0)
                    ),
                    Set((1, 0)),
                    Set((2, 0)),
                    (0, (1, 0), (0, 0))
                ),
                (
                    Array(
                        Array(3, 4, 0),
                        Array(2, 3, 4),
                        Array(2, 4, 0)
                    ),
                    Set((1, 1)),
                    Set((2, 0)),
                    (0, (1, 1), (0, 0))
                ),
                (
                    Array(
                        Array(2, 4, 1),
                        Array(2, 3, 4),
                        Array(2, 4, 0)
                    ),
                    Set((1, 1)),
                    Set((2, 0)),
                    (0, (1, 1), (0, 2))
                ),
                (
                    Array(
                        Array(2, 4, 0),
                        Array(3, 3, 4),
                        Array(2, 4, 0)
                    ),
                    Set((1, 1)),
                    Set((2, 0)),
                    (0, (1, 1), (1, 0))
                ),
                (
                    Array(
                        Array(2, 4, 0),
                        Array(2, 3, 4),
                        Array(2, 4, 1)
                    ),
                    Set((1, 1)),
                    Set((2, 0)),
                    (0, (1, 1), (2, 2))
                )
            ), 16)
        },
        /* What the better move for
        4 3 4 4
        3 3 3 0
        4 0 4 3
        3 2 1 3
        OWN 1 1
        ENE 0 1
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(4, 3, 4, 4),
                        Array(3, 4, 3, 0),
                        Array(4, 0, 4, 3),
                        Array(3, 2, 1, 3)
                    ),
                    Set((1, 0)),
                    Set((0, 1)),
                    (0, (1, 0), (1, 1))
                ),
                (
                    Array(
                        Array(4, 3, 4, 4),
                        Array(3, 3, 3, 0),
                        Array(4, 1, 4, 3),
                        Array(3, 2, 1, 3)
                    ),
                    Set((1, 0)),
                    Set((0, 1)),
                    (0, (1, 0), (2, 1))
                ),
                (
                    Array(
                        Array(4, 3, 4, 4),
                        Array(3, 3, 3, 1),
                        Array(4, 0, 4, 3),
                        Array(3, 2, 1, 3)
                    ),
                    Set((1, 2)),
                    Set((0, 1)),
                    (0, (1, 2), (1, 3))
                ),
                (
                    Array(
                        Array(4, 3, 4, 4),
                        Array(3, 4, 3, 0),
                        Array(4, 0, 4, 3),
                        Array(3, 2, 1, 3)
                    ),
                    Set((1, 2)),
                    Set((0, 1)),
                    (0, (1, 2), (1, 1))
                ),
                (
                    Array(
                        Array(4, 3, 4, 4),
                        Array(3, 4, 3, 0),
                        Array(4, 0, 4, 3),
                        Array(3, 2, 1, 3)
                    ),
                    Set((2, 1)),
                    Set((0, 1)),
                    (0, (2, 1), (1, 1))
                )
            ), 17)
        },
        /* What the better move for
        0 3 3 4 4
        0 0 0 2 4
        0 0 3 3 1
        0 0 3 0 4
        4 0 2 4 4
        OWN 0 2, 4 1
        ENE 3 2, 3 3
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(1, 3, 3, 4, 4),
                        Array(0, 0, 0, 2, 4),
                        Array(0, 0, 3, 3, 1),
                        Array(0, 0, 3, 0, 4),
                        Array(4, 0, 2, 4, 4)
                    ),
                    Set((0, 1), (4, 1)),
                    Set((3, 2), (3, 3)),
                    (0, (0, 1), (0, 0))
                ),
                (
                    Array(
                        Array(0, 3, 3, 4, 4),
                        Array(0, 0, 0, 3, 4),
                        Array(0, 0, 3, 3, 1),
                        Array(0, 0, 3, 0, 4),
                        Array(4, 0, 2, 4, 4)
                    ),
                    Set((1, 2), (4, 1)),
                    Set((3, 2), (3, 3)),
                    (0, (1, 2), (1, 3))
                )
            ), 18)
        },
        /* What the better move for
        1 1 3 4
        1 2 3 3
        0 2 4 3
        1 1 3 0
        OWN 1 0, 2 1
        ENE 1 3, 3 3
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(1, 2, 3, 4),
                        Array(1, 2, 3, 3),
                        Array(0, 2, 4, 3),
                        Array(1, 1, 3, 0)
                    ),
                    Set((1, 0), (1, 2)),
                    Set((1, 3), (3, 3)),
                    (0, (1, 2), (0, 1))
                ),
                (
                    Array(
                        Array(1, 1, 3, 4),
                        Array(1, 2, 3, 3),
                        Array(1, 2, 4, 3),
                        Array(1, 1, 3, 0)
                    ),
                    Set((1, 0), (3, 0)),
                    Set((1, 3), (3, 3)),
                    (0, (3, 0), (2, 0))
                ),
                (
                    Array(
                        Array(1, 1, 3, 4),
                        Array(1, 2, 3, 3),
                        Array(0, 2, 4, 3),
                        Array(1, 2, 3, 0)
                    ),
                    Set((1, 0), (3, 0)),
                    Set((1, 3), (3, 3)),
                    (0, (3, 0), (3, 1))
                )
            ), 19)
        },
        /* What the better move for
        1 1 3 4
        1 2 3 3
        0 2 4 3
        1 1 3 0
        OWN 1 0, 2 1
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(1, 2, 3, 4),
                        Array(1, 2, 3, 3),
                        Array(0, 2, 4, 3),
                        Array(1, 1, 3, 0)
                    ),
                    Set((1, 0), (1, 2)),
                    Set[(Int, Int)](),
                    (0, (1, 2), (0, 1))
                ),
                (
                    Array(
                        Array(1, 1, 3, 4),
                        Array(1, 2, 3, 3),
                        Array(1, 2, 4, 3),
                        Array(1, 1, 3, 0)
                    ),
                    Set((1, 0), (3, 0)),
                    Set[(Int, Int)](),
                    (0, (3, 0), (2, 0))
                ),
                (
                    Array(
                        Array(1, 1, 3, 4),
                        Array(1, 2, 3, 3),
                        Array(0, 2, 4, 3),
                        Array(1, 2, 3, 0)
                    ),
                    Set((1, 0), (3, 0)),
                    Set[(Int, Int)](),
                    (0, (3, 0), (3, 1))
                )
            ), 19)
        },
        /* What the better move for
        1 3 1
        2 1 2
        4 2 1
        OWN 1 0
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(1, 3, 2),
                        Array(2, 1, 2),
                        Array(4, 2, 1)
                    ),
                    Set((0, 1)),
                    Set[(Int, Int)](),
                    (0, (0, 1), (0, 2))
                ),
                (
                    Array(
                        Array(1, 3, 2),
                        Array(2, 1, 2),
                        Array(4, 2, 1)
                    ),
                    Set((1, 1)),
                    Set[(Int, Int)](),
                    (0, (1, 1), (0, 2))
                )
            ), 20)
        },
        /* What the better move for
        1 1 3 4
        1 2 3 3
        0 2 4 3
        1 1 3 0
        OWN 1 0, 2 1
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(1, 2, 3, 4),
                        Array(1, 2, 3, 3),
                        Array(0, 2, 4, 3),
                        Array(1, 1, 3, 0)
                    ),
                    Set((1, 0), (1, 2)),
                    Set[(Int, Int)](),
                    (0, (1, 2), (0, 1))
                ),
                (
                    Array(
                        Array(1, 1, 3, 4),
                        Array(1, 2, 3, 3),
                        Array(1, 2, 4, 3),
                        Array(1, 1, 3, 0)
                    ),
                    Set((1, 0), (3, 0)),
                    Set[(Int, Int)](),
                    (0, (3, 0), (2, 0))
                ),
                (
                    Array(
                        Array(1, 1, 3, 4),
                        Array(1, 2, 3, 3),
                        Array(0, 2, 4, 3),
                        Array(1, 2, 3, 0)
                    ),
                    Set((1, 0), (3, 0)),
                    Set[(Int, Int)](),
                    (0, (3, 0), (3, 1))
                )
            ), 21)
        },
        /* What the better move for
        0 0 1 3 0
        0 2 2 1 4
        0 0 1 0 0
        0 0 0 0 0
        0 0 0 0 0
        OWN 2 1, 2 3
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(0, 0, 1, 3, 0),
                        Array(0, 2, 2, 1, 4),
                        Array(0, 0, 1, 0, 0),
                        Array(0, 0, 1, 0, 0),
                        Array(0, 0, 0, 0, 0)
                    ),
                    Set((2, 2), (2, 3)),
                    Set[(Int, Int)](),
                    (0, (2, 2), (3, 2))
                ),
                (
                    Array(
                        Array(0, 0, 1, 3, 0),
                        Array(0, 2, 2, 1, 4),
                        Array(0, 0, 1, 0, 0),
                        Array(0, 0, 1, 0, 0),
                        Array(0, 0, 0, 0, 0)
                    ),
                    Set((3, 1), (2, 3)),
                    Set[(Int, Int)](),
                    (0, (3, 1), (3, 2))
                ),
                (
                    Array(
                        Array(0, 0, 1, 3, 0),
                        Array(0, 2, 2, 1, 4),
                        Array(0, 0, 1, 0, 0),
                        Array(0, 0, 0, 0, 0),
                        Array(1, 0, 0, 0, 0)
                    ),
                    Set((3, 1), (2, 3)),
                    Set[(Int, Int)](),
                    (0, (3, 1), (4, 0))
                ),
                (
                    Array(
                        Array(0, 0, 1, 3, 0),
                        Array(0, 2, 2, 1, 4),
                        Array(0, 1, 1, 0, 0),
                        Array(0, 0, 0, 0, 0),
                        Array(1, 0, 0, 0, 0)
                    ),
                    Set((3, 2), (2, 3)),
                    Set[(Int, Int)](),
                    (0, (3, 2), (2, 1))
                )
            ), 22)
        },
        /* What the better move for
        0 0 0 0 0
        0 0 0 0 0
        2 1 4 0 0
        2 3 2 1 0
        0 0 1 1 0
        OWN 3 2, 2 3
        ENE 3 1, 4 4
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(0, 0, 0, 0, 0),
                        Array(0, 0, 0, 0, 0),
                        Array(2, 1, 4, 0, 0),
                        Array(2, 4, 2, 1, 0),
                        Array(0, 0, 1, 1, 0)
                    ),
                    Set((3, 2), (2, 3)),
                    Set((4, 0), (4, 4)),
                    (1, (3, 1), (4, 0))
                ),
                (
                    Array(
                        Array(0, 0, 0, 0, 0),
                        Array(0, 0, 0, 0, 1),
                        Array(2, 1, 4, 0, 0),
                        Array(2, 3, 2, 1, 0),
                        Array(0, 0, 1, 1, 0)
                    ),
                    Set((3, 2), (1, 3)),
                    Set((3, 1), (4, 4)),
                    (0, (1, 3), (1, 4))
                )
            ), 23)
        },
        /* What the better move for
        0 2 4 4
        1 2 4 3
        1 1 3 3
        1 4 1 2
        OWN 1 0, 1 3
        ENE 2 2
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(0, 2, 4, 4),
                        Array(1, 2, 4, 3),
                        Array(1, 1, 3, 3),
                        Array(1, 4, 2, 2)
                    ),
                    Set((1, 0), (2, 3)),
                    Set((2, 2)),
                    (0, (2, 3), (3, 2))
                ),
                (
                    Array(
                        Array(0, 2, 4, 4),
                        Array(1, 2, 4, 3),
                        Array(1, 1, 4, 3),
                        Array(1, 4, 1, 2)
                    ),
                    Set((1, 0), (1, 3)),
                    Set((2, 1)),
                    (1, (2, 2), (2, 1))
                )
            ), 24)
        },
        /* What the better move for
        3 4 4 3 2 3
        3 4 4 3 4 4
        1 2 1 1 2 2
        0 0 4 4 0 1
        0 0 0 0 0 0
        0 0 0 0 0 0
        OWN 1 3, 2 5
        ENE 0 0, 0 5
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(3, 4, 4, 3, 2, 3),
                        Array(3, 4, 4, 3, 4, 4),
                        Array(1, 2, 1, 1, 2, 2),
                        Array(0, 0, 4, 4, 1, 1),
                        Array(0, 0, 0, 0, 0, 0),
                        Array(0, 0, 0, 0, 0, 0)
                    ),
                    Set((1, 3), (2, 4)),
                    Set((0, 0), (0, 5)),
                    (0, (2, 4), (3, 4))
                ),
                (
                    Array(
                        Array(3, 4, 4, 3, 2, 3),
                        Array(3, 4, 4, 3, 4, 4),
                        Array(1, 2, 1, 1, 2, 2),
                        Array(0, 0, 4, 4, 1, 1),
                        Array(0, 0, 0, 0, 0, 0),
                        Array(0, 0, 0, 0, 0, 0)
                    ),
                    Set((1, 3), (3, 5)),
                    Set((0, 0), (0, 5)),
                    (0, (3, 5), (3, 4))
                )
            ), 25)
        },
        /* What the better move for
        0 1 3 3
        4 4 4 4
        4 4 4 4
        4 4 4 4
        OWN 0 2
        */
        // {
        //     testscore(Array(
        //         (
        //             Array(
        //                 Array(1, 1, 3, 3),
        //                 Array(4, 4, 4, 4),
        //                 Array(4, 4, 4, 4),
        //                 Array(4, 4, 4, 4)
        //             ),
        //             Set((0, 1)),
        //             Set[(Int, Int)](),
        //             (0, (0, 1), (0, 0))
        //         ),
        //         (
        //             Array(
        //                 Array(0, 1, 4, 3),
        //                 Array(4, 4, 4, 4),
        //                 Array(4, 4, 4, 4),
        //                 Array(4, 4, 4, 4)
        //             ),
        //             Set((0, 1)),
        //             Set[(Int, Int)](),
        //             (0, (0, 1), (0, 2))
        //         )
        //     ), 26)
        // },
        /* What the better move for
        0 1 3 3
        4 4 4 4
        4 4 4 4
        4 4 4 4
        OWN 0 2, 1 0
        */
        {
            testscore(Array(
                (
                    Array(
                        Array(4, 3, 3, 1, 0, 0),
                        Array(3, 3, 4, 4, 0, 0),
                        Array(4, 4, 4, 4, 4, 4),
                        Array(4, 4, 4, 4, 4, 4),
                        Array(4, 4, 4, 4, 4, 4),
                        Array(4, 4, 4, 4, 4, 4)
                    ),
                    Set((0, 1), (1, 0)),
                    Set[(Int, Int)](),
                    (0, (0, 1), (0, 0))
                ),
                (
                    Array(
                        Array(3, 3, 4, 1, 0, 0),
                        Array(3, 3, 4, 4, 0, 0),
                        Array(4, 4, 4, 4, 4, 4),
                        Array(4, 4, 4, 4, 4, 4),
                        Array(4, 4, 4, 4, 4, 4),
                        Array(4, 4, 4, 4, 4, 4)
                    ),
                    Set((0, 1)),
                    Set[(Int, Int)](),
                    (0, (0, 1), (0, 2))
                )
            ), 27)
        }
    ).mkString("\n\n"))
