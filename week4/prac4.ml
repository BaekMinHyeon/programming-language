type state = Q0 | Q1 | Q2 | Q3 | Q4 | Q5 | Q6 | Q7 | Q8 | Q9 | Q10 | Q11 | Q12 | Q13 | Q14 | Q15 | Q16 | Q17 | Q18 | Q19 | Q20

let start_state = Q0

let is_final_state (s: state) : string option =
        match s with
        | Q11 -> Some "cnu"
        | Q20 -> Some "gmail"
        | _ -> None

let transfer_func (s: state) (c: char) : state option =
        match s, c with
        | Q0, 'a'..'z' -> Some Q1
        | Q0, _ -> None
        | Q1, '_' -> Some Q1
        | Q1, 'a'..'z' -> Some Q1
        | Q1, 'A'..'Z' -> Some Q1
        | Q1, '@' -> Some Q2
        | Q1, _ -> None
        | Q2, 'c' -> Some Q3
        | Q2, 'g' -> Some Q12
        | Q2, _ -> None
        | Q3, 'n' -> Some Q4
        | Q3, _ -> None
        | Q4, 'u' -> Some Q5
        | Q4, _ -> None
        | Q5, '.' -> Some Q6
        | Q5, _ -> None
        | Q6, 'a' -> Some Q7
        | Q6, _ -> None
        | Q7, 'c' -> Some Q8
        | Q7, _ -> None
        | Q8, '.' -> Some Q9
        | Q8, _ -> None
        | Q9, 'k' -> Some Q10
        | Q9, _ -> None
        | Q10, 'r' -> Some Q11
        | Q10, _ -> None
        | Q11, _ -> None
        | Q12, 'm' -> Some Q13
        | Q12, _ -> None
        | Q13, 'a' -> Some Q14
        | Q13, _ -> None
        | Q14, 'i' -> Some Q15
        | Q14, _ -> None
        | Q15, 'l' -> Some Q16
        | Q15, _ -> None
        | Q16, '.' -> Some Q17
        | Q16, _ -> None
        | Q17, 'c' -> Some Q18
        | Q17, _ -> None
        | Q18, 'o' -> Some Q19
        | Q18, _ -> None
        | Q19, 'm' -> Some Q20
        | Q19, _ -> None
        | Q20, _ -> None

let cnu_email_lex (str: string) : bool =
        let cur_state = start_state in
        let cur_index = 0 in
        let char_list = List.of_seq (String.to_seq str) in
        let rec fa (cur_state: state) (cur_index: int) : bool =
                let c = List.nth char_list cur_index in
                match transfer_func cur_state c with
                | None -> false
                | Some s -> let next_index = cur_index + 1 in
                            begin
                                    if next_index = (List.length char_list) then
                                            begin
                                                    if (is_final_state s = Some "cnu") then true
                                                    else false
                                            end
                                    else fa s next_index
                            end
        in
        fa cur_state cur_index

let email_lex (str: string) : string option =
        let cur_state = start_state in
        let cur_index = 0 in
        let char_list = List.of_seq (String.to_seq str) in
        let rec fa (cur_state: state) (cur_index: int) : bool =
                let c = List.nth char_list  cur_index in
                match transfer_func cur_state c with
                | None -> false
                | Some s -> let next_index = cur_index + 1 in
                            begin
                                    if next_index = (List.length char_list) then
                                            begin
                                                    if (is_final_state s = Some "gmail") then true
                                                    else false
                                            end
                                    else fa s next_index
                            end
        in
        if cnu_email_lex str = true then Some "cnu"
        else if fa cur_state cur_index = true then Some "gmail"
        else None
