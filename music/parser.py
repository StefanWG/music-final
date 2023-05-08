import json
import os

def breakUpFile(fp):
    with open(fp, "r") as file:
        lines = file.readlines()
        x = 0
        for i in range(len(lines) // 1000):
            with open(f"bach-data/batch{i}.txt", "w") as out:
                for _ in range(1000):
                    out.write(lines[x])
                    x+=1


def aldaToString(alda, feedback):
    s = f"{{:genome ["
    for note in alda:
        s += f"{{:note {note['note']} :duration {int(note['duration'])}}} "
    s += f"] :feedback {feedback}}}"
    return s

# break up big bach-data file into small ones so i can push to git
# breakUpFile("data.txt")

melodies = []

for filePath in os.listdir("bach-data"):
    with open(f'bach-data/{filePath}', "r") as file:
        melodies.extend(file.readlines())

with open("src/melodies.txt", "w") as file:
    for melody in melodies:
        mJson = json.loads(melody)
        notes = mJson["output_sequence"][0]["notes"]
        notes = [n for n in notes if "instrument" not in n]
        if mJson["feedback"][0] == "":
            continue
        alda = []
        for n in notes:
            if "quantizedStartStep" in n:
                duration = int(n["quantizedEndStep"]) - int(n["quantizedStartStep"])
            else:
                duration = int(n["quantizedEndStep"]) 
            duration = (1 / duration) * 4
            if duration < 1:
                continue
            alda.append({"note": n["pitch"], "duration": duration})
        feedback = 2 - int(mJson["feedback"][0])
        file.write(aldaToString(alda, feedback))
        file.write("\n")
