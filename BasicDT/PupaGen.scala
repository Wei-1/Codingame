class Feature(val n: String, val t: String, var std: Double = 1.0) {
    override def toString: String = n + " " + t
}
class Pupa(val i: Int, val s: Int, val f: Array[Int]) {
    override def toString: String = i + " " + s + " " + f.mkString(" ")
}

object PupaGen extends App {

    def calCenter(iarr: Array[(Int, Array[Int])]): Array[(Int, Int, Array[Int])] =
        iarr.groupBy(_._1).map { case (i, arr) =>
            val ac = arr.size
            val as = arr.map(_._2).reduce((a, b) => a.zip(b).map(ab => ab._1 + ab._2))
            (i, ac, as.map(_ / ac))
        }.toArray
    def getCenteri(centers: Array[(Int, Int, Array[Int])], f: Array[Int]): Int =
        centers.minBy { case (i, c, arr) =>
            arr.zip(f).map { case (a, b) =>
                (a - b).abs
            }.zip(fs).map { case (a, fi) =>
                if(fi.t == "category") {
                    if(a > 0.1) 1.0 else 0.0
                } else {
                    a / fi.std
                }
            }.sum * Math.sqrt(Math.sqrt(c)) // Special weight to keep each group balance
        }._1
    def specialKMean(k: Int, fSet: Array[Array[Int]], limit: Int): Array[(Int, Array[Int])] = {
        var fSeti = fSet.zipWithIndex.map { case (f, i) => ((i % k) + 1, f) }
        var fAvgi = calCenter(fSeti)
        fAvgi.foreach { case (i, c, arr) => println(s"Group:$i N:$c") }
        for(i <- 0 until limit) {
            fSeti = fSeti.map { case (i, f) => (getCenteri(fAvgi, f), f) }
            fAvgi = calCenter(fSeti)
            fAvgi.foreach { case (i, c, arr) => println(s"Group:$i N:$c") }
        }
        fSeti
    }
    def randGenFeatures: Array[Int] = fs.map { f =>
        if(f.t == "category") {
            Math.random * 4
        } else {
            val c = Math.pow(Math.random, 2) * 5000
            Math.pow((Math.random - 0.5) * 2, 3) * c + c
        }
    }.map(_.toInt).toArray

    val tpn = 10
    val vpn = 10
    val rpn = 10
    val sn = 2
    val fs = Array(
        ("a", 1),
        ("b", 1)
    ).map { case (name, typei) =>
        new Feature(name, if(typei == 0) "category" else "numerical")
    }
    val fn = fs.size

    println(s"Number of Features: $fn")
    println(s"Number of Training Pupae: $tpn")
    println(s"Number of Validation Pupae: $vpn")
    println(s"Number of Real testing Pupae: $rpn")
    println(s"Number of Species: $sn")
    val fSet = Array.fill(900 * sn)(randGenFeatures)
    for(i <- 0 until fs.size) {
        val vSet = fSet.map(_(i))
        val vSize = vSet.size
        val vAvg = vSet.sum / vSize.toDouble
        val vStd = Math.sqrt(vSet.map(v => Math.pow(v - vAvg, 2)).sum)
        fs(i).std = vStd
    }
    val speciesFSet = specialKMean(sn, fSet, 10).groupBy(_._1).map(kv => (kv._1, kv._2.map(_._2))).toMap

    println("----")
    println(s"$fn $tpn $vpn $rpn")
    fs.foreach(println)

    val tps = (0 until tpn).map { i =>
        val pIG = (tpn / sn) + 1
        val gid = (i / pIG) + 1
        new Pupa(i, gid, speciesFSet(gid)(i))
    }
    tps.foreach(println)

    val vps = (tpn until tpn + vpn).map { i =>
        val pIG = (vpn / sn) + 1
        val gid = ((i - tpn) / pIG) + 1
        new Pupa(i, gid, speciesFSet(gid)(i))
    }
    vps.foreach(println)

    val rps = (tpn + vpn until tpn + vpn + rpn).map { i =>
        val pIG = (rpn / sn) + 1
        val gid = ((i - tpn - vpn) / pIG) + 1
        new Pupa(i, gid, speciesFSet(gid)(i))
    }
    rps.foreach(println)
}