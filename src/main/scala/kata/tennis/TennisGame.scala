package kata.tennis

sealed trait Player
case class Player1() extends Player
case class Player2() extends Player

sealed trait Score
case class Love() extends Score
case class Fifteen() extends Score
case class Thirty() extends Score
case class Forty() extends Score
case class Wins() extends Score
case class Advantage() extends Score

case class TennisGame(player1: Score, player2: Score) {

  def play(winner: Player): TennisGame = {
    winner match {
      case Player1() if points(player1,player2)._1 != Wins() => {
        val p = points(player1, player2)
        TennisGame(p._1, p._2)
      }
      case Player2() if points(player2,player1)._1 != Wins() => {
        val p = points(player2, player1)
        TennisGame(p._2, p._1)
      }
      case _ => TennisGame(Love(), Love())
    }
  }

  private def points(winner: Score, looser: Score): (Score, Score) = winner match {
    case Love() => (Fifteen(), looser)
    case Fifteen() => (Thirty(), looser)
    case Thirty() => (Forty(), looser)
    case Forty() if looser == Advantage() => (Forty(), Forty())
    case Forty() if looser != Forty() => (Wins(), looser)
    case Forty() if looser == Forty() => (Advantage(), looser)
    case Advantage() if looser != Advantage() => (Wins(), looser)
    case Advantage() if looser == Advantage() => (Forty(), Forty())
  }

}


