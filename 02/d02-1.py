# Scores for shapes "X", "Y", "Z"
shape_scores = [1, 2, 3]

# Scores for loss, draw, win
outcome_scores = [0, 3, 6]

score = 0
fp = open("input.dat", "r")
for line in fp:
    first_choice_ind = ord(line[0]) - ord("A")
    second_choice_ind = ord(line[2]) - ord("X")

    # Bring 2nd choice into the middle of the 0-2 interval (to position 1)
    # Shift 1st choice by the same amount
    # Fold 1st choice into the 0-2 interval
    # Calculate difference between shifted 2nd and 1st choices  (-1, 0 or 1)
    # Shift difference into the interval [0, 2] to match outcome_score entry
    outcome_ind = 1 - ((first_choice_ind + (1 - second_choice_ind)) % 3) + 1
    score = score + shape_scores[second_choice_ind] + outcome_scores[outcome_ind]
fp.close()

print("Score:", score)
