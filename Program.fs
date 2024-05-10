open System
open System.Threading


// Task 1 - type Account with the required details. 
type Account = {
    accountNumber : string
    mutable balance : float
    withdrawal : float -> unit // withdrawal method.
    deposit : float -> unit // deposit method.
    print : unit -> unit // print method.
}

// Helper Function to create an account with accountNumber and initialBalance
let createAccount accountNumber initialBalance =
    let rec account = {
        accountNumber = accountNumber
        balance = initialBalance
        withdrawal = fun amount -> // withdrawal method with proper conditions
            if amount <= 0.0 || amount > account.balance then
                printfn "Invalid withdrawal amount"
            else
                account.balance <- account.balance - amount
        deposit = fun amount -> // deposit methods with proper conditions
            if amount <= 0.0 then
                printfn "Invalid deposit amount"
            else
                account.balance <- account.balance + amount
        print = fun () -> printfn "Account Number: %s, Balance: %.2f" account.accountNumber account.balance // printing method.
    }
    account

let myAccount = createAccount "1234567890" 1000.0
myAccount.print()
myAccount.withdrawal(200.0)
myAccount.deposit(500.0)
myAccount.print()

// TASK 2
// Function checkAccount to check the account balance and display appropriate message
let checkAccount (account : Account) =
    match account.balance with
    | b when b < 10.0 -> printfn "Balance is low"
    | b when b >= 10.0 && b <= 100.0 -> printfn "Balance is OK"
    | _ -> printfn "Balance is high"

// Example usage
let user1 = createAccount "1234" 0.0
let user2 = createAccount "4321" 51.0
let user3 = createAccount "1122" 105.0
checkAccount user1
checkAccount user2
checkAccount user3

// TASK 3
// Creating six accounts
let userAccounts =
    [
        createAccount "0001" 10.0
        createAccount "0002" 20.0
        createAccount "0003" 30.0
        createAccount "0004" 40.0
        createAccount "0005" 50.0
        createAccount "0006" 60.0
    ]


// Sequencing based on the given criteria
let accountsLessThan50 = userAccounts |> Seq.filter (fun acc -> acc.balance >= 0.0 && acc.balance < 50.0)
let accounts50OrMore = userAccounts |> Seq.filter (fun acc -> acc.balance >= 50.0)

// Print the sequences
printfn "Accounts with balance less than 50:"
accountsLessThan50 |> Seq.iter (fun acc -> acc.print())
printfn ""
printfn "Accounts with balance 50 or more:"
accounts50OrMore |> Seq.iter (fun acc -> acc.print())

// Task 4 - Threading Part
// Defining the type as given in the brief
type Ticket = { seat : int; mutable customer : string }

// Mutable list of tickets with initial values
let mutable tickets = [for n in 1..10 -> { seat = n; customer = "" }]

// This function will display the list of tickets
let displayTickets () =
    printfn "Tickets:"
    tickets |> List.iter (fun ticket -> printfn "Seat: %d, Customer: %s" ticket.seat ticket.customer)

// Function to book a seat for a customer
let bookSeat (customerName : string) (seatNumber : int) =
    lock tickets (fun () ->
        if seatNumber < 1 || seatNumber > 10 then // checking for the valid seat number
            printfn "Invalid seat number. Please choose a seat between 1 and 10."
        elif tickets.[seatNumber - 1].customer <> "" then // checking if the seat is already booked
            printfn "Seat %d is already booked by %s." seatNumber tickets.[seatNumber - 1].customer
        else // if nothing goes wrong, ticket is booked. 
            tickets <- tickets |> List.mapi (fun i ticket ->
                if i = seatNumber - 1 then { ticket with customer = customerName }
                else ticket
            )
            printfn "Seat %d booked for %s." seatNumber customerName
    )

// threads for booking seats
let thread1 = new Thread(fun () -> bookSeat "Rayan" 3)
let thread2 = new Thread(fun () -> bookSeat "Dayan" 7)

// Starting the threads
thread1.Start()
thread2.Start()

// Waiting for the threads to finish
thread1.Join()
thread2.Join()

// Display the updated list of tickets
displayTickets()


// Add this line to pause the execution
System.Console.ReadLine() |> ignore