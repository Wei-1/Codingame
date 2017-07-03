
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

    def dist(p1: (Int, Int), p2: (Int, Int)): Double = {
        return Math.max((p1._1 - p2._1).abs, (p1._2 - p2._2).abs)
    }

    def eneestimate(oa: (Int, (Int, Int), (Int, Int)),
        og: Map[(Int, Int), Int], ng: Map[(Int, Int), Int],
        om: Set[(Int, Int)], nm: Set[(Int, Int)],
        oe: Set[(Int, Int)], ne: Set[(Int, Int)]): Set[(Int, Int)] = {
        val nesize = ne.size
        val oesize = oe.size
        val (ot, od, ob) = oa
        if (nesize == 2) {
            return ne
        } else if (nesize == 1 || nesize == 0) {
            var fe = ne
            val diffk = ng.toArray.map { case (k, v) =>
                (k, v - og.getOrElse(k, 4))
            }.filter(_._2 != 0)
            val diffksize = diffk.size
            if (diffksize == 0) {
                // 1. enemy dead
                // 2. enemy fail when move or push (should be next to me)
                // 3. I fail when move or push cus I didn't know another enemy is right behind (and then the enemy build on the same place)
                /* Case 3 Details List
                    1. the build place should be od
                    2. If I push then there should be an enemy at ob whatever
                    3. If I move then enemy should be around od
                */
                if (nesize == 0) {
                    // guess enemmy dead
                    return oe
                } else if (nesize == 1) {
                    // If the enemy around my unit had not move then dead
                    val theene = ne.head
                    if (oe.contains(theene)) {
                        // probably dead
                        return oe
                    } else {
                        // if the enemy move
                        if (oesize == 2) {
                            val otherene = oe.maxBy(p => dist(p, theene))
                            val farmecheck = nm.forall(p => dist(p, otherene) > 1)
                            if (farmecheck) {
                                fe += otherene
                            }
                        } else if (oesize == 1) {
                            val otherene = oe.head
                            val thedist = dist(otherene, theene)
                            val farmecheck = nm.forall(p => dist(p, otherene) > 1)
                            if (thedist > 1 && farmecheck) {
                                fe += otherene
                            }
                        }
                    }
                }
            } else if (diffksize == 2) {
                val Array(missact, nextact) = diffk.sortBy(_._2)
                if (missact._2 == -1 && nextact._2 == 1) {
                    // 1. I fail when move or push cus I didn't know another enemy is right behind (and then the enemy build on somewhere else)
                    val fromp = missact._1
                    val atlevel = ng.getOrElse(fromp, 4)
                    val (fpy, fpx) = fromp
                    val top = nextact._1
                    val (tpy, tpx) = top
                    // The one that block my act must have move to otherwhere else
                    if (nesize == 0) {
                        val possiblep = dirarr.map { case (dy, dx) =>
                            val p = (fpy + dy, fpx + dx)
                            val tolevel = ng.getOrElse(p, 4)
                            if (tolevel < atlevel + 2 && tolevel < 4) {
                                val farmecheck = nm.forall(m => dist(m, p) > 1)
                                if (farmecheck) {
                                    val (py, px) = p
                                    val crosscheck = dirarr.exists { case (ddy, ddx) =>
                                        tpy + ddy == py && tpx + ddx == px
                                    }
                                    if (crosscheck) (p, tolevel)
                                    else (p, -1)
                                } else (p, -1)
                            } else (p, -1)
                        }.filter(_._2 >= 0)
                        // have possible move
                        if (possiblep.size > 0) {
                            if (ot == 0) {
                                // if I moved
                                fe += possiblep.maxBy(_._2)._1
                            } else if (ot == 1) {
                                // if I pushed
                                fe += ob
                                val nextpossiblep = possiblep.filter(_._1 != ob)
                                if (nextpossiblep.size > 0) {
                                    fe += nextpossiblep.maxBy(_._2)._1
                                }
                            }
                        } else if (ot == 1) {
                            fe += ob
                        }
                    } else if (nesize == 1) {
                        val theene = ne.head
                        if (oe.contains(theene)) {
                            // then the one that is missing should be ->
                            val possiblep = dirarr.map { case (dy, dx) =>
                                val p = (fpy + dy, fpx + dx)
                                val tolevel = ng.getOrElse(p, 4)
                                if (tolevel < atlevel + 2 && tolevel < 4) {
                                    val farmecheck = nm.forall(m => dist(m, p) > 1)
                                    if (farmecheck) {
                                        val (py, px) = p
                                        val crosscheck = dirarr.exists { case (ddy, ddx) =>
                                            tpy + ddy == py && tpx + ddx == px
                                        }
                                        if (crosscheck) (p, tolevel)
                                        else (p, -1)
                                    } else (p, -1)
                                } else (p, -1)
                            }.filter(_._2 >= 0)
                            if (possiblep.size > 0 && ot == 0) {
                                fe += possiblep.maxBy(_._2)._1
                            }
                        } else {
                            if (ot == 0) {
                                if (oesize == 2) {
                                    val otherene = oe.maxBy(p => dist(p, theene))
                                    val farmecheck = nm.forall(p => dist(p, otherene) > 1)
                                    if (farmecheck) {
                                        fe += otherene
                                    }
                                } else if (oesize == 1) {
                                    val otherene = oe.head
                                    if (dist(otherene, theene) > 1) {
                                        val farmecheck = nm.forall(p => dist(p, otherene) > 1)
                                        if (farmecheck) {
                                            fe += otherene
                                        }
                                    }
                                }
                            } else if (ot == 1) {
                                fe += ob
                            }
                        }
                    }
                }
            } else if (diffksize == 1) {
                val (dk, dv) = diffk.head
                if (dv == 1) {
                    // 1. enemy simply build on dk
                    val igotpushed = !nm.forall(p => om.contains(p))
                    if (igotpushed) {
                        // Console.err.println("I GOT PUSHED : " + oe.mkString(", "))
                        return oe
                    } else if (nesize == 0) {
                        val (py, px) = dk
                        val possiblep = dirarr.map { case (dy, dx) =>
                            val p = (py + dy, px + dx)
                            val tolevel = ng.getOrElse(p, 4)
                            val mind = nm.map(pp => dist(p, pp)).min
                            if (tolevel < 4 && mind > 1) (p, tolevel)
                            else (p, -1)
                        }.filter(_._2 >= 0).toArray
                        if (possiblep.size > 0) {
                            if (oesize == 2) {
                                if (oe.contains(dk)) { // if someone that I predicted stand on the build location it should move
                                    val otherene = oe.filter(_ != dk).head
                                    val tarr = possiblep.filter(_._1 != otherene)
                                    if (tarr.size > 0) {
                                        fe += tarr.maxBy(_._2)._1
                                        fe += otherene
                                    }
                                } else {
                                    val tarr = possiblep.map { case (p, v) =>
                                        (p, v, oe.map { pp =>
                                            val oev = ng.getOrElse(pp, 4)
                                            if (oev < v + 2) {
                                                (pp, dist(p, pp))
                                            } else {
                                                (pp, 10000)
                                            }
                                        }.filter(_._2 == 1))
                                    }.filter(_._3.size > 0)
                                    if (tarr.size > 0) {
                                        // val Seq(farene, nearene) = oe.toSeq.sortBy(p => dist(p, dk)) /////// NEED CHECK ///////
                                        fe += tarr.maxBy { case (p, v, arr) =>
                                            val mind = nm.map(pp => dist(p, pp)).min
                                            v * 3 - mind
                                        }._1
                                        fe += oe.maxBy(p => dist(p, dk))
                                    } else {
                                        fe += possiblep.maxBy { case (p, v) =>
                                            val mind = nm.map(pp => dist(p, pp)).min
                                            val ened = oe.map(pp => dist(p, pp)).min
                                            v * 3 - mind - ((ened - 1).abs * 10)
                                        }._1
                                        fe += oe.maxBy(p => dist(p, dk))
                                    }
                                }
                            } else if (oesize == 1) {
                                val theene = oe.head
                                val (otarr, ntarr) = possiblep.map { case (p, v) =>
                                    val mind = nm.map(pp => dist(p, pp)).min
                                    val ened = dist(theene, p)
                                    if (ened == 1) {
                                        (p, v * 3 - mind + 10000)
                                    } else {
                                        (p, v * 3 - mind)
                                    }
                                }.partition(_._2 > 5000)
                                if (otarr.size > 0) {
                                    fe += otarr.maxBy(_._2)._1
                                } else {
                                    fe += theene
                                    fe += ntarr.maxBy(_._2)._1
                                }
                            } else {
                                fe += possiblep.maxBy { case (p, v) =>
                                    val mind = nm.map(pp => dist(p, pp)).min
                                    v * 3 - mind
                                }._1
                            }
                        }
                    } else if (nesize == 1) {
                        val (py, px) = dk
                        val theene = ne.head
                        val possiblep = dirarr.map { case (dy, dx) =>
                            val p = (py + dy, px + dx)
                            val tolevel = ng.getOrElse(p, 4)
                            val mind = nm.map(pp => dist(p, pp)).min
                            if (tolevel < 4 && mind > 1) (p, tolevel)
                            else (p, -1)
                        }.filter(_._2 >= 0)
                        if (oe.contains(theene)) { // theene is not the one who moved
                            fe += theene
                            if (oesize == 2) {
                                val otherene = oe.filter(_ != theene).head
                                val otherenelevel = ng.getOrElse(otherene, 4)
                                if (possiblep.size > 0) {
                                    fe += possiblep.maxBy { case (p, v) =>
                                        val mind = nm.map(pp => dist(p, pp)).min
                                        val ened = dist(otherene, p)
                                        if (otherenelevel > v - 2) {
                                            v * 3 - mind - ((ened - 1).abs * 10)
                                        } else -10000
                                    }._1
                                }
                            } else if (oesize == 1) {
                                if (possiblep.size > 0) {
                                    fe += possiblep.maxBy { case (p, v) =>
                                        val mind = nm.map(pp => dist(p, pp)).min
                                        v * 3 - mind
                                    }._1
                                }
                            }
                        } else { // theene is the one who moved
                            if (oesize == 2) {
                                val (otarr, ntarr) = oe.partition(p => dist(theene, p) == 1)
                                if (otarr.size == 2) {
                                    val tarr = otarr.filter(p => nm.map(pp => dist(p, pp)).min > 1)
                                    if (tarr.size > 0) {
                                        fe += tarr.head
                                    }
                                } else if (otarr.size == 1 && ntarr.size == 1) {
                                    fe += ntarr.head
                                } else {
                                    fe += oe.maxBy(p => dist(theene, p))
                                }
                            } else if (oesize == 1) {
                                val otherene = oe.head
                                if (dist(otherene, theene) > 1) {
                                    fe += otherene
                                }
                            }
                        }
                    }
                } else if (dv == -1) {
                    // 1. I fail when move or build and enemy were dead
                    // 2. I fail when move or build and enemy also fail when move or push (should be next to me)
                    if (ot == 1) {
                        fe += ob
                    }
                }
            }
            return fe.takeRight(2)
        } else {
            return ne.takeRight(2)
        }
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

    def testpredict(oa: (Int, (Int, Int), (Int, Int)),
        tog: Array[Array[Int]], tng: Array[Array[Int]],
        om: Set[(Int, Int)], nm: Set[(Int, Int)],
        oe: Set[(Int, Int)], ne: Set[(Int, Int)],
        reale: Set[(Int, Int)],
        caseid: Int): String = {
        val og = arr2map(tog)
        val ng = arr2map(tng)
        val testr = eneestimate(oa, og, ng, om, nm, oe, ne)
        testr.mkString(",") + "\n" +
            "TEST CASE " + caseid + " " +
            (if (reale.forall(p => testr.contains(p))) "SUCCESS"
            else "ERROR")
    }

    Console.err.println(Array("\n\nTESTS",
        {
            testpredict(
                (0, (1, 1), (2, 0)),
                Array(
                    Array(0, 1, 0),
                    Array(4, 1, 4),
                    Array(4, 1, 3)
                ),
                Array(
                    Array(0, 1, 0),
                    Array(4, 2, 4),
                    Array(4, 1, 3)
                ),
                Set((1, 1)),
                Set((0, 0)),
                Set((2, 1)),
                Set[(Int, Int)](),
                Set((2, 1)),
                1
            )
        },
        {
            testpredict(
                (0, (0, 0), (0, 1)),
                Array(
                    Array(0, 1, 0),
                    Array(0, 0, 0),
                    Array(0, 0, 0)
                ),
                Array(
                    Array(0, 1, 0),
                    Array(0, 0, 0),
                    Array(0, 0, 1)
                ),
                Set((0, 0)),
                Set((0, 0)),
                Set((1, 0)),
                Set[(Int, Int)](),
                Set((2, 1)),
                2
            )
        },
        {
            testpredict(
                (0, (0, 0), (0, 1)),
                Array(
                    Array(0, 1, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0)
                ),
                Array(
                    Array(0, 1, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 1)
                ),
                Set((0, 0)),
                Set((0, 0)),
                Set((1, 1)),
                Set[(Int, Int)](),
                Set((2, 2)),
                3
            )
        },
        {
            testpredict(
                (0, (0, 0), (0, 1)),
                Array(
                    Array(0, 1, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0)
                ),
                Array(
                    Array(0, 1, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 1)
                ),
                Set((0, 0)),
                Set((0, 0)),
                Set[(Int, Int)](),
                Set[(Int, Int)](),
                Set((2, 2)),
                4
            )
        },
        {
            testpredict(
                (1, (2, 2), (3, 3)),
                Array(
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 1, 0),
                    Array(0, 0, 0, 0)
                ),
                Array(
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0)
                ),
                Set((0, 0), (1, 1)),
                Set((0, 0), (1, 1)),
                Set((3, 3)),
                Set((2, 2)),
                Set((2, 2), (3, 3)),
                5
            )
        },
        {
            testpredict(
                (1, (2, 2), (3, 3)),
                Array(
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 1, 0),
                    Array(0, 0, 0, 0)
                ),
                Array(
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(1, 0, 0, 0)
                ),
                Set((0, 0), (1, 1)),
                Set((0, 0), (1, 1)),
                Set((3, 3)),
                Set[(Int, Int)](),
                Set((3, 1), (3, 3)),
                6
            )
        },
        {
            testpredict(
                (1, (2, 2), (3, 3)),
                Array(
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 1, 0),
                    Array(0, 0, 0, 0)
                ),
                Array(
                    Array(0, 0, 0, 0),
                    Array(0, 1, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0)
                ),
                Set((1, 0), (1, 1)),
                Set((1, 0), (0, 0)),
                Set((3, 3)),
                Set[(Int, Int)](),
                Set((2, 2), (3, 3)),
                7
            )
        },
        {
            testpredict(
                (1, (2, 2), (3, 3)),
                Array(
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 1, 0),
                    Array(0, 0, 0, 0)
                ),
                Array(
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 1, 1),
                    Array(0, 0, 0, 0)
                ),
                Set((1, 0), (1, 1)),
                Set((1, 0), (1, 1)),
                Set((0, 1), (3, 3)),
                Set((0, 1)),
                Set((0, 1), (3, 2)),
                8
            )
        },
        {
            testpredict(
                (1, (2, 2), (2, 3)),
                Array(
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 1, 0),
                    Array(0, 0, 3, 3)
                ),
                Array(
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 1, 1),
                    Array(0, 0, 3, 3)
                ),
                Set((1, 0), (1, 1)),
                Set((1, 0), (1, 1)),
                Set((0, 1), (2, 3)),
                Set((0, 1)),
                Set((0, 1), (1, 3)),
                9
            )
        },
        {
            testpredict(
                (1, (2, 2), (3, 3)),
                Array(
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 2, 0),
                    Array(0, 3, 3, 3)
                ),
                Array(
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 2, 0),
                    Array(0, 3, 3, 3)
                ),
                Set((1, 0), (1, 1)),
                Set((1, 0), (1, 1)),
                Set((3, 3)),
                Set[(Int, Int)](),
                Set((3, 3)),
                10
            )
        },
        {
            testpredict(
                (1, (2, 2), (2, 3)),
                Array(
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 2, 0),
                    Array(0, 3, 3, 3)
                ),
                Array(
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 2, 0),
                    Array(0, 3, 3, 3)
                ),
                Set((1, 0), (1, 1)),
                Set((1, 0), (1, 1)),
                Set((2, 3)),
                Set[(Int, Int)](),
                Set((2, 3)),
                11
            )
        },
        {
            testpredict(
                (0, (0, 0), (0, 1)),
                Array(
                    Array(0, 1, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0)
                ),
                Array(
                    Array(0, 1, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0)
                ),
                Set((1, 0), (0, 0)),
                Set((1, 0), (0, 0)),
                Set[(Int, Int)](),
                Set[(Int, Int)](),
                Set[(Int, Int)](),
                12
            )
        },
        {
            testpredict(
                (0, (0, 0), (0, 1)),
                Array(
                    Array(0, 1, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0)
                ),
                Array(
                    Array(0, 1, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0)
                ),
                Set((1, 0), (0, 0)),
                Set((1, 0), (0, 0)),
                Set((3, 3)),
                Set[(Int, Int)](),
                Set((3, 3)),
                13
            )
        },
        {
            testpredict(
                (0, (0, 0), (0, 1)),
                Array(
                    Array(0, 1, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0)
                ),
                Array(
                    Array(0, 1, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0),
                    Array(0, 0, 0, 0)
                ),
                Set((1, 0), (0, 0)),
                Set((1, 0), (0, 0)),
                Set((3, 2), (3, 3)),
                Set[(Int, Int)](),
                Set((3, 2), (3, 3)),
                14
            )
        }
    ).mkString("\n\n"))
