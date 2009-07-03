

type memo = {r_base:char;
  mutable ala :string Mlpeg.result option;
  mutable rule1 :string Mlpeg.result option;
  mutable rule2 :string Mlpeg.result option;
  mutable rule3 :string Mlpeg.result option;
  mutable a_main :string Mlpeg.result option;
}

let rec memo_table_init ch = {
  r_base=ch;
  ala= None;
  rule1= None;
  rule2= None;
  rule3= None;
  a_main= None;
}

and ala stream = 
  let __memo = (Mstream.top stream) in
  match __memo.ala with
  | Some a -> begin match a with | Mlpeg.Success(p,r) -> Printf.printf "ReadCached: %s" r;Mstream.advance stream p; a | a -> a end
  | None ->
  begin
    Mstream.push stream;
    let loc = 
    "ala"
     in
    begin
    if (Mstream.next stream).r_base = 'a' then
    if (Mstream.next stream).r_base = 'l' then
    if (Mstream.next stream).r_base = 'a' then
    begin
      Mstream.drop stream;
      __memo. ala<- Some ( 
      Mlpeg.Success ((Mstream.spos stream), loc)
      );
      Mlpeg.Success ((Mstream.spos stream), loc)
    end
    else
    begin
      Mstream.pop stream;
      __memo. ala<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. ala<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. ala<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    end
  end

and rule1 stream = 
  let __memo = (Mstream.top stream) in
  match __memo.rule1 with
  | Some a -> begin match a with | Mlpeg.Success(p,r) -> Printf.printf "ReadCached: %s" r;Mstream.advance stream p; a | a -> a end
  | None ->
  begin
    Mstream.push stream;
    match ala stream with
      | Mlpeg.Fail -> 
    begin
      Mstream.pop stream;
      __memo. rule1<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
      | Mlpeg.Success(_, loc) -> 
    let loc = 
    "ma kota"
     in
    begin
    if (Mstream.next stream).r_base = 'm' then
    if (Mstream.next stream).r_base = 'a' then
    if (Mstream.next stream).r_base = ' ' then
    if (Mstream.next stream).r_base = 'k' then
    if (Mstream.next stream).r_base = 'o' then
    if (Mstream.next stream).r_base = 't' then
    if (Mstream.next stream).r_base = 'a' then
    begin
      Mstream.drop stream;
      __memo. rule1<- Some ( 
      Mlpeg.Success ((Mstream.spos stream), loc)
      );
      Mlpeg.Success ((Mstream.spos stream), loc)
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule1<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule1<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule1<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule1<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule1<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule1<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule1<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    end
  end

and rule2 stream = 
  let __memo = (Mstream.top stream) in
  match __memo.rule2 with
  | Some a -> begin match a with | Mlpeg.Success(p,r) -> Printf.printf "ReadCached: %s" r;Mstream.advance stream p; a | a -> a end
  | None ->
  begin
    Mstream.push stream;
    match ala stream with
      | Mlpeg.Fail -> 
    begin
      Mstream.pop stream;
      __memo. rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
      | Mlpeg.Success(_, loc) -> 
    let loc = 
    "nie ma kota"
     in
    begin
    if (Mstream.next stream).r_base = 'n' then
    if (Mstream.next stream).r_base = 'i' then
    if (Mstream.next stream).r_base = 'e' then
    if (Mstream.next stream).r_base = ' ' then
    if (Mstream.next stream).r_base = 'm' then
    if (Mstream.next stream).r_base = 'a' then
    if (Mstream.next stream).r_base = ' ' then
    if (Mstream.next stream).r_base = 'k' then
    if (Mstream.next stream).r_base = 'o' then
    if (Mstream.next stream).r_base = 't' then
    if (Mstream.next stream).r_base = 'a' then
    begin
      Mstream.drop stream;
      __memo. rule2<- Some ( 
      Mlpeg.Success ((Mstream.spos stream), loc)
      );
      Mlpeg.Success ((Mstream.spos stream), loc)
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    end
  end

and rule3 stream = 
  let __memo = (Mstream.top stream) in
  match __memo.rule3 with
  | Some a -> begin match a with | Mlpeg.Success(p,r) -> Printf.printf "ReadCached: %s" r;Mstream.advance stream p; a | a -> a end
  | None ->
  begin
    Mstream.push stream;
    match ala stream with
      | Mlpeg.Fail -> 
    begin
      Mstream.pop stream;
      __memo. rule3<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
      | Mlpeg.Success(_, loc) -> 
    let loc = 
    "to kot"
     in
    begin
    if (Mstream.next stream).r_base = 't' then
    if (Mstream.next stream).r_base = 'o' then
    if (Mstream.next stream).r_base = ' ' then
    if (Mstream.next stream).r_base = 'k' then
    if (Mstream.next stream).r_base = 'o' then
    if (Mstream.next stream).r_base = 't' then
    begin
      Mstream.drop stream;
      __memo. rule3<- Some ( 
      Mlpeg.Success ((Mstream.spos stream), loc)
      );
      Mlpeg.Success ((Mstream.spos stream), loc)
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule3<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule3<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule3<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule3<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule3<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      __memo. rule3<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    end
  end

and a_main stream = 
  let __memo = (Mstream.top stream) in
  match __memo.a_main with
  | Some a -> begin match a with | Mlpeg.Success(p,r) -> Printf.printf "ReadCached: %s" r;Mstream.advance stream p; a | a -> a end
  | None ->
  begin
    Mstream.push stream;
    begin
      Mstream.push stream;
      match rule1 stream with
        | Mlpeg.Fail -> 
      begin
        Mstream.pop stream;
        begin
          Mstream.push stream;
          match rule2 stream with
            | Mlpeg.Fail -> 
          begin
            Mstream.pop stream;
            match rule3 stream with
              | Mlpeg.Fail -> 
            begin
              Mstream.pop stream;
              __memo. a_main<- Some ( 
              Mlpeg.Fail
              );
              Mlpeg.Fail
            end
              | Mlpeg.Success(_, loc) -> 
            begin
              Mstream.drop stream;
              __memo. a_main<- Some ( 
              Mlpeg.Success ((Mstream.spos stream), loc)
              );
              Mlpeg.Success ((Mstream.spos stream), loc)
            end
          end
            | Mlpeg.Success(_, loc) -> 
          begin
            Mstream.drop stream;
            __memo. a_main<- Some ( 
            Mlpeg.Success ((Mstream.spos stream), loc)
            );
            Mlpeg.Success ((Mstream.spos stream), loc)
          end
        end
      end
        | Mlpeg.Success(_, loc) -> 
      begin
        Mstream.drop stream;
        __memo. a_main<- Some ( 
        Mlpeg.Success ((Mstream.spos stream), loc)
        );
        Mlpeg.Success ((Mstream.spos stream), loc)
      end
    end
  end

let parse str = (Mlpeg.parse memo_table_init str a_main)