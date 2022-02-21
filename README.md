# R_Diceroller
A complex-dice-roller script written in R (the script is not terribly complex)

### Purpose:
This script prompts the user for a complex dice-roll to simulate and does a few things: It...
- Interprets and re-writes the user's input, handling a number of potential input errors
- Can provide a number of details/statistics related to the roll specifications provided including...
  - Individual dice roll results for each simulated die
  - Expected roll amount (average), variance, & standard deviation for the combined roll
  - A small blurb about how much of an outlier the roll is if it's in the top or bottom ~15% of possible results 
    - The middle 68%, i.e. within 1 standard deviation is considered 'average' for this blurb

### Valid inputs / Use:
- User supplies dice-roll specifications to simulate (in the form [?]d[?] or +/-[?]), with each 'term' separated by either + or -
  - e.x.  5d6 means 5x 6-sided dice (with values 1-6)
- Valid characters: numbers 0-9, +, -, d   
  - integers only, '.' is not a valid character, you can't have fractional dice or fractions of a dice-roll
- Specifications can contain any number of terms including repeated dice sizes of any integer amount
  - e.x.  '2d5 + 12 - 1d6 + 4d4 + d17 + 4d6 + 2d6 - 4 + 2d137 ...'
- If +/-'s are omitted then spaces are interpreted as +'s, if dice quantities are omitted they're interpreted as 1d?
  - e.x.  '2d5 d6 7' is seen as '2d5 + 1d6 + 7'

### The script will not:...
- Will not combine/collapse like terms (5d6 + 2d6 will not become 7d6 though 7x d6's will be rolled in total)
- Will not show pity on you after multiple bad rolls (It's random, maybe you just have bad luck)
