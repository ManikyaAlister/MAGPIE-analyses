#!/bin/bash

MODELS_DIR="R/analyse/lm-output/posts-seen/models"

declare -A label_map
label_map["Condition"]="cond"
label_map["prop_right_all"]="pra"
label_map["prop_left_all"]="pla"
label_map["prop_right_troll"]="prt"
label_map["prop_left_troll"]="plt"
label_map["Condition+prop_right_all"]="cond_pra"
label_map["Condition+prop_left_all"]="cond_pla"
label_map["Condition+prop_right_all+prop_left_all"]="cond_pra_pla"
label_map["prop_right_all+prop_left_all"]="pra_pla"
label_map["Condition+prop_right_troll+prop_left_troll"]="cond_prt_plt"
label_map["prop_right_troll+prop_left_troll"]="prt_plt"
label_map["prop_right_nontroll+prop_left_nontroll"]="prnt_plnt"
label_map["prop_right_troll+prop_left_troll+prop_right_nontroll+prop_left_nontroll"]="prt_plt_prnt_plnt"

for old_path in "$MODELS_DIR"/*.rds; do
  [ -f "$old_path" ] || continue
  filename=$(basename "$old_path" .rds)

  variable="${filename%%__*}"
  old_label="${filename##*__}"

  new_label="${label_map[$old_label]}"

  if [ -z "$new_label" ]; then
    echo "No mapping found for: $old_label — skipping"
    continue
  fi

  new_path="$MODELS_DIR/${variable}__${new_label}.rdata"
  mv "$old_path" "$new_path"
  echo "Renamed: $filename.rds → ${variable}__${new_label}.rdata"
done