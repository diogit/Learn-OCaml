let wellformed date =
  date.year >= 1
  && date.month >= 1 && date.month <= 5
  && date.day >= 1 && date.day <= 4
  && date.hour >= 0 && date.hour <= 2
  && date.minute >= 0 && date.minute <= 1;;

let next date = 
  let nextMinute = { year = date.year;
                     month = date.month;
                     day = date.day;
                     hour = date.hour;
                     minute = date.minute + 1
                   } in
  if wellformed nextMinute then nextMinute else
    let nextHour = { year = date.year;
                     month = date.month;
                     day = date.day;
                     hour = date.hour + 1;
                     minute = date.minute - 1
                   } in
    if wellformed nextHour then nextHour else
      let nextDay = { year = date.year;
                      month = date.month;
                      day = date.day + 1;
                      hour = date.hour - 2;
                      minute = date.minute - 1
                    } in
      if wellformed nextDay then nextDay else
        let nextMonth = { year = date.year;
                          month = date.month + 1;
                          day = date.day - 3;
                          hour = date.hour - 2;
                          minute = date.minute - 1
                        } in
        if wellformed nextMonth then nextMonth else
          { year = date.year + 1;
            month = date.month - 4;
            day = date.day - 3;
            hour = date.hour - 2;
            minute = date.minute - 1
          }
      
;;

let of_int minutes =
  let rec
    computeDate minutes date =
    if minutes = 0
    then date
    else computeDate (minutes - 1) (next date)
  in computeDate minutes the_origin_of_time
