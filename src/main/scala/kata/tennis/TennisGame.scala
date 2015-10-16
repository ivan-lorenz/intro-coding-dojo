package kata.tennis

sealed trait Player
case object Player1 extends Player
case object Player2 extends Player

sealed trait Score {
  def wins(looser: Score): (Score, Score) = this match {
    case Love => (Fifteen, looser)
    case Fifteen => (Thirty, looser)
    case Thirty => (Forty, looser)
    case Forty if looser == Advantage => (Forty, Forty)
    case Forty if looser != Forty => (Love, Love)
    case Forty if looser == Forty => (Advantage, looser)
    case Advantage if looser != Advantage => (Love, Love)
    case Advantage if looser == Advantage => (Forty, Forty)
  }
}

case object Love extends Score
case object Fifteen extends Score
case object Thirty extends Score
case object Forty extends Score
case object Wins extends Score
case object Advantage extends Score

case class TennisGame(player1: Score, player2: Score) {

  def play(winner: Player): TennisGame = {
    winner match {
      case Player1 => {
        val p = player1 wins player2
        TennisGame(p._1, p._2)
      }
      case Player2 => {
        val p = player2 wins player1
        TennisGame(p._2, p._1)
      }
    }
  }

}

