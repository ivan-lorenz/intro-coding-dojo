package kata.tennis

import org.scalatest.{ShouldMatchers, FlatSpec}


class TennisGameSpec extends FlatSpec with ShouldMatchers {

  behavior of "Tennis Game"

  it should "change state to Fifteen-Love from Love-Love" in {
    val game = TennisGame(Love(), Love())
    assert(game.play(Player1()) === TennisGame(Fifteen(),Love()))
  }

  it should "change state to Fifteen-Fifteen from Fifteen-love" in {
    val game = TennisGame(Fifteen(), Love())
    assert(game.play(Player2()) === TennisGame(Fifteen(),Fifteen()))
  }

  it should "change state to Forty-Forty from Thirty-Forty" in {
    val game = TennisGame(Thirty(), Forty())
    assert(game.play(Player1()) === TennisGame(Forty(),Forty()))
  }

  it should "start a new game if one player wins" in {
    val game = TennisGame(Thirty(), Forty())
    assert(game.play(Player2()) === TennisGame(Love(), Love()))
  }

  it should "change state to Forty-Advantage from Forty-Forty" in {
    val game = TennisGame(Forty(), Forty())
    assert(game.play(Player2()) === TennisGame(Forty(), Advantage()))
  }

  it should "start a new game if one player wins from Advantage" in {
    val game = TennisGame(Forty(), Advantage())
    assert(game.play(Player2()) === TennisGame(Love(), Love()))
  }

  it should "change state to Deuce-Deuce from Forty-Advantage" in {
    val game = TennisGame(Forty(), Advantage())
    assert(game.play(Player1()) === TennisGame(Forty(), Forty()))
  }

  it should "change state to Deuce-Advantage from Deuce-Deuce" in {
    val game = TennisGame(Forty(), Forty())
    assert(game.play(Player2()) === TennisGame(Forty(), Advantage()))
  }

  it should "start a new game from Deuce-Advantage" in {
    val game = TennisGame(Forty(), Advantage())
    assert(game.play(Player2()) === TennisGame(Love(), Love()))
  }


}
