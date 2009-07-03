

type memo = {r_base:char;
  mutable ala :string Mlpeg.result option;
  mutable rule1 :string Mlpeg.result option;
  mutable rule2 :string Mlpeg.result option;
  mutable rule3 :string Mlpeg.result option;
  mutable a_main :int Mlpeg.result option;
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
  let loc = (Mstream.top stream).ala in
  match loc with
  | Some a -> Printf.printf "ReadCached: ala
"; a
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
      (Mstream.top stream). ala<- Some ( 
      Mlpeg.Success ((Mstream.spos stream), loc)
      );
      Mlpeg.Success ((Mstream.spos stream), loc)
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). ala<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). ala<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). ala<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    end
  end

and rule1 stream = 
  let loc = (Mstream.top stream).rule1 in
  match loc with
  | Some a -> Printf.printf "ReadCached: rule1
"; a
  | None ->
  begin
    Mstream.push stream;
    match ala stream with
      | Mlpeg.Fail -> 
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule1<- Some ( 
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
      (Mstream.top stream). rule1<- Some ( 
      Mlpeg.Success ((Mstream.spos stream), loc)
      );
      Mlpeg.Success ((Mstream.spos stream), loc)
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule1<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule1<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule1<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule1<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule1<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule1<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule1<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    end
  end

and rule2 stream = 
  let loc = (Mstream.top stream).rule2 in
  match loc with
  | Some a -> Printf.printf "ReadCached: rule2
"; a
  | None ->
  begin
    Mstream.push stream;
    match ala stream with
      | Mlpeg.Fail -> 
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule2<- Some ( 
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
      (Mstream.top stream). rule2<- Some ( 
      Mlpeg.Success ((Mstream.spos stream), loc)
      );
      Mlpeg.Success ((Mstream.spos stream), loc)
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule2<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    end
  end

and rule3 stream = 
  let loc = (Mstream.top stream).rule3 in
  match loc with
  | Some a -> Printf.printf "ReadCached: rule3
"; a
  | None ->
  begin
    Mstream.push stream;
    match ala stream with
      | Mlpeg.Fail -> 
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule3<- Some ( 
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
      (Mstream.top stream). rule3<- Some ( 
      Mlpeg.Success ((Mstream.spos stream), loc)
      );
      Mlpeg.Success ((Mstream.spos stream), loc)
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule3<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule3<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule3<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule3<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule3<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    else
    begin
      Mstream.pop stream;
      (Mstream.top stream). rule3<- Some ( 
      Mlpeg.Fail
      );
      Mlpeg.Fail
    end
    end
  end

and a_main stream = 
  let loc = (Mstream.top stream).a_main in
  match loc with
  | Some a -> Printf.printf "ReadCached: a_main
"; a
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
              (Mstream.top stream). a_main<- Some ( 
              Mlpeg.Fail
              );
              Mlpeg.Fail
            end
              | Mlpeg.Success(_, loc) -> 
            let loc = 
             1 
             in
            begin
            begin
              Mstream.drop stream;
              (Mstream.top stream). a_main<- Some ( 
              Mlpeg.Success ((Mstream.spos stream), loc)
              );
              Mlpeg.Success ((Mstream.spos stream), loc)
            end
            end
          end
            | Mlpeg.Success(_, loc) -> 
          let loc = 
           1 
           in
          begin
          begin
            Mstream.drop stream;
            (Mstream.top stream). a_main<- Some ( 
            Mlpeg.Success ((Mstream.spos stream), loc)
            );
            Mlpeg.Success ((Mstream.spos stream), loc)
          end
          end
        end
      end
        | Mlpeg.Success(_, loc) -> 
      let loc = 
       1 
       in
      begin
      begin
        Mstream.drop stream;
        (Mstream.top stream). a_main<- Some ( 
        Mlpeg.Success ((Mstream.spos stream), loc)
        );
        Mlpeg.Success ((Mstream.spos stream), loc)
      end
      end
    end
  end

let parse str = (Mlpeg.parse memo_table_init str a_main)