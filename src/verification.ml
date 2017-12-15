let do_init filename node_names =
  List.iter (fun n ->
      let ch = Unix.open_process_in (Format.sprintf "why3 prove %s -T Node%s -t 10 -G prop_init -a split_goal_wp  -a split_goal_wp -P Z3" filename n)
      in
      let ok = ref true in
      begin
        try
          while true do
            let s = input_line ch in
            try
              let i = String.index s ':' in
              if String.contains_from s i 'U' || String.contains_from s i 'T' then
                ok := false
              else if not (String.contains_from s i 'V') then
                raise Not_found
            with
            | Not_found -> Format.printf "Invalid why3 output: @\n";
              print_endline s;
          done;
        with
        | End_of_file -> ()
      end;
      let stat = Unix.close_process_in ch in

      begin
        match stat with
        | Unix.WEXITED(0) ->
          if !ok then
            Format.printf "%s: property \027[32mOK\027[0m at init@." n
          else
            Format.printf "%s: property \027[31mNOT OK\027[0m at init@." n
        | _ -> Format.printf "%s: property checking failed@." n
      end;
      let ch = Unix.open_process_in (Format.sprintf "why3 prove %s -T Node%s -t 10 -G prop_ind -a split_goal_wp  -a split_goal_wp -P Z3" filename n)
      in
      let ok = ref true in
      begin
        try
          while true do
            let s = input_line ch in
            try
              let i = String.index s ':' in
              if String.contains_from s i 'U' || String.contains_from s i 'T' then
                ok := false
              else if not (String.contains_from s i 'V') then
                raise Not_found
            with
            | Not_found -> Format.printf "Invalid why3 output: @\n@.";
              print_endline s;
          done;
        with
        | End_of_file -> ()
      end;
      let stat = Unix.close_process_in ch in

      match stat with
      | Unix.WEXITED(0) ->
        if !ok then
          Format.printf "%s: property \027[32mOK\027[0m at recurrence@." n
        else
          Format.printf "%s: property \027[31mNOT OK\027[0m  at recurrence@." n
      | _ -> Format.printf "%s: property checking failed@." n

    ) node_names;
