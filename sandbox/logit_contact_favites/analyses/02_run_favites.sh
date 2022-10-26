#!/usr/bin/env bash



for i in {1..2}
do
  sed "s/mycontact_out_ret/mycontact_out_ret_${i}/g" ../gears/template_play_contact_network_config.json > ../gears/play_contact_network_config_${i}.json
  sed "s/favites_realized_contact_matrix/favites_realized_contact_matrix_${i}/g" ../gears/play_contact_network_config_${i}.json > ../gears/play_contact_network_config_${i}.json
  python3 ../engine/FAVITES/run_favites_docker.py --config $i
done
