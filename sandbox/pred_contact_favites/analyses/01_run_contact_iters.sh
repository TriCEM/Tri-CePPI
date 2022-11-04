#!/usr/bin/env bash



for i in {1..1000}
do
  Rscript ../R/realize_contact_matrix.R -I $i
  sleep 2 # file latency
  sed "s/mycontact_out_ret/mycontact_out_ret_${i}/g" ../gears/template_play_contact_network_config.json > temp.json
  sed "s/favites_realized_contact_matrix/favites_realized_contact_matrix_${i}/g" temp.json > ../gears/play_contact_network_config_${i}.json
  rm temp.json
  sleep 2 # file latency
  python3 ../engine/FAVITES/run_favites_docker.py --config ../gears/play_contact_network_config_${i}.json
done
