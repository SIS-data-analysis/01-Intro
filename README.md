# Solving the Monty Hall Problem in R

Austin Hart, American University

## The Monty Hall problem
This classic probability puzzle is based loosely on Monty Hall’s game show *Let’s Make a Deal*. The problem as we know it now was made famous in 1990 as a reader's question to Marilyn vos Savant (of *Parade* magazine’s "Ask Marilyn"). The reader framed the problem this way:

> Suppose you're on a game show, and you're given the choice of three doors: Behind one door is a car; behind the others, goats. You pick a door, say No. 1, and the host, who knows what's behind the doors, opens another door, say No. 3, revealing a goat. He then says to you, "Do you want to pick door No. 2?" Is it to your advantage to switch your choice?

Marilyn’s answer was simple: switch. Switching doubles the probability of winning the car: $Pr[car|switch] = 0.666$; $Pr[car|stay] = 0.333$. Her answer was not popular... The magazine received more than 10,000 letters insisting that vos Savant was wrong and a total fool for suggesting that the contestant switch. Marilyn's follow-up explanations and
formal proofs were not enough to satisfy the most serious doubters. Notably, mathematician Paul Erös rejected Marilyn's answer until he was shown a computer simulation.

## Your challenge
Write and execute a computer simulation to convince Marilyn's most skeptical readers that switching doors is the best solution to the Monty Hall problem. Code the routine using the R language.
