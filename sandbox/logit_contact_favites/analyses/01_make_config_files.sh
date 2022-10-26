#!/usr/bin/env bash

for i in {1..100}
do
   sed "s/mycontact_out_ret/mycontact_out_ret_${i}/g" ../gears/template_play_contact_network_config.json > ../gears/play_contact_network_config_${i}.json
done
