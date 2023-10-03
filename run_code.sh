#!/usr/bin/env bash


cd DB
# konsole --hold -e bash -c 'docker-compose up &'
# konsole --noclose -e 'docker-compose up' &
konsole -e 'docker-compose up' &
cd ..

# konsole --noclose -e "websocat -v ws://127.0.0.1:1234" &
konsole &
konsole &

exec dbeaver &
exec code . &


# workspace_one=1
# workspace_db_number=9
# workspace_websocat_number=5

# i3-msg "workspace $workspace_websocat_number; exec konsole &"
# i3-msg "workspace $workspace_websocat_number; exec konsole &"


# cd DB
# i3-msg "workspace $workspace_db_number; exec konsole --hold -e bash -c 'docker-compose up' &"
# cd ..

# i3-msg "workspace $workspace_one; exec dbeaver &"
# i3-msg "workspace $workspace_one; exec code . &"




# #!/usr/bin/env nix-shell
# #!nix-shell -p python38Full -p python38Packages.virtualenv
# #!nix-shell -i bash
# source .venv/bin/activate

# # insert commands to be run here

