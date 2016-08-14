import os
import json


def prepare_workspaces_data():
    json_output = os.popen("i3-msg -t get_workspaces").read()
    workspaces_raw = json.loads(json_output)
    return sorted(workspaces_raw, key=lambda ws: ws["num"])


def get_workspaces():
    workspaces = prepare_workspaces_data()
    result = ""
    for workspace in workspaces:
        if workspace["visible"]:
            result += '%{F#e8586e}'
        elif workspace["focused"]:
            result += '%{F#616161}'

        result += workspace["name"] + " "

    print(result)

if __name__ == '__main__':
    get_workspaces()
