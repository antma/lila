package lila.tournament
package arena

import PairingSystem.{ Data, url }

private object AntmaPairing {

  def apply(data: Data, players: RankedPlayers): List[Pairing.Prep] = players.nonEmpty ?? {
    import data._

    val a: Array[RankedPlayer] = players.toArray
    val n: Int = a.length

    def justPlayTogether(u1: String, u2: String) =
      recentOpponents.justPlayTogether(u1, u2)

    def pairScore(i: Int, j: Int): Int =
      Math.abs(a(i).rank - a(j).rank) * 1000 +
      Math.abs(a(i).player.rating - a(j).player.rating) +
      (recentOpponents.score(a(i).player.userId, a(j).player.userId) * 8e6 + 0.5).toInt

    players match {
      case x if x.size < 2 => Nil
      case List(p1, p2) if onlyTwoActivePlayers => List(Pairing.prep(tour, p1.player, p2.player))
      case List(p1, p2) if justPlayTogether(p1.player.userId, p2.player.userId) => Nil
      case _ => {
        try {
          val mate = WMMatching.minWeightMatching(WMMatching.fullGraph(n, pairScore))
          WMMatching.mateToEdges(mate).map { x =>
            Pairing.prep(tour, a(x._1).player, a(x._2).player)
          }
        }
        catch {
          case e: Exception =>
            logger.error("AntmaPairing", e)
            Nil
        }
      }
    }
  }
}
