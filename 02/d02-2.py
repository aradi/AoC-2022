# Scores for shapes "X", "Y", "Z"
shape_scores = [1, 2, 3]

# Scores for loss, draw, win
outcome_scores = [0, 3, 6]

score = 0
fp = open("input.dat", "r")
for line in fp:
    first_choice_ind = ord(line[0]) - ord("A")
    outcome_ind = ord(line[2]) - ord("X")
    # Map outcome index onto [-1, 0, 1]
    outcome_shift = outcome_ind - 1
    second_choice_ind = (first_choice_ind + outcome_shift) % 3
    score = score + shape_scores[second_choice_ind] + outcome_scores[outcome_ind]
fp.close()

print("Score:", score)
