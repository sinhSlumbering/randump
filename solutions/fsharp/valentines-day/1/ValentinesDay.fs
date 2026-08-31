module ValentinesDay

// TODO: please define the 'Approval' discriminated union type
type Approval = 
| Maybe
| Yes
| No

// TODO: please define the 'Cuisine' discriminated union type
type Cusine = 
| Korean
| Turkish

// TODO: please define the 'Genre' discriminated union type
type Genre = 
| Crime
| Horror
| Romance
| Thriller

// TODO: please define the 'Activity' discriminated union type
type Activity =
| BoardGame
| Chill
| Movie of Genre
| Restaurant of Cusine
| Walk of int

let rateActivity (activity: Activity): Approval =
    match activity with
    | Walk lenInKm -> 
        if lenInKm < 3 then Approval.Yes
        elif lenInKm < 5 then Approval.Maybe
        else Approval.No
    | Movie Romance     -> Approval.Yes
    | Restaurant cusine -> 
        match cusine with
        | Turkish -> Approval.Maybe
        | Korean  -> Approval.Yes
    | Movie _
    | Chill
    | BoardGame -> Approval.No