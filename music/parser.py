import json

def aldaToString(alda, feedback):
    s = f"{{:genome ["
    for note in alda:
        s += f"{{:note {note['note']} :duration {int(note['duration'])}}} "
    s += f"] :feedback {feedback}}}"
    return s


with open("data.txt", "r") as file:
    melodies = file.readlines()

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
