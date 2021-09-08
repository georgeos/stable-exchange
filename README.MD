# Stable Exchange

Allows to exchange an amount of ADA by "MyCoin" Tokens, only if there is a deposit of ADA. Relation is 1 ADA - 1 "MyCoin"

Technically, this logic involves a script validator and a minting policy. Minting policy only mints if there is a deposit of ADA in the Script validator.

## Script validator

### Redeemer
- Deposit: Validates outputValue is greather than inputValue
- Redeem: Validates if the transaction was signed by owner

## Minting policy
- Validates if there is an input for script validator.

### Notes
This is just a practice trying to include many concepts as possible from Pioneer program.

Disclaimer: This project shouldn't be used in a production environment because there are some holes in the code. E.g. This doesn't validates the amount to be minted against deposited amount.