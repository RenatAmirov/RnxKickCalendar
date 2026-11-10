module KickCalendar.Program

open System
open System.Collections.Generic
open System.Globalization
open System.IO
open System.Reflection
open System.Windows
open System.Windows.Automation
open System.Windows.Controls
open System.Windows.Markup
open System.Windows.Media
open System.Windows.Shapes
open System.Windows.Threading
open KickCalendar.Calendar

let private russian = CultureInfo.GetCultureInfo "ru-RU"
let private monthNames = [| "ЯНВ"; "ФЕВ"; "МАР"; "АПР"; "МАЙ"; "ИЮН"; "ИЮЛ"; "АВГ"; "СЕН"; "ОКТ"; "НОЯ"; "ДЕК" |]

let private brush color =
    let value = BrushConverter().ConvertFromString(color: string) :?> Brush
    value.Freeze()
    value

let private ink = brush "#242821"
let private muted = brush "#625E54"
let private accent = brush "#9B382A"
let private rule = brush "#DED8CA"
let private paper = brush "#FFFCF5"
let private cream = brush "#FFF1DA"
let private marked = brush "#DDE3D4"
let private markedLine = brush "#A9AF9F"
let private future = brush "#F8F5EE"
let private futureInk = brush "#726B5F"
let private weekend = brush "#F1EBDF"
let private morning = brush "#FFF2E4"
let private morningLine = brush "#E2C7B4"
let private morningFill = brush "#E4A88C"
let private crossed = brush "#A6B19F"

let private halfDay = Geometry.Parse "M 0,20 L 24,0 L 0,0 Z"
let private fullDay = Geometry.Parse "M 2,18 L 22,2 M 2,2 L 22,18"

let private control<'T> (window: Window) name = window.FindName(name) :?> 'T
let private style (window: Window) name = window.FindResource(name) :?> Style
let private visible yes = if yes then Visibility.Visible else Visibility.Collapsed
let private calendarDate () = DateOnly.FromDateTime DateTime.Today

let private describeMark = function
    | Mark.Morning -> "отмечено утро"
    | Mark.Complete -> "зачёркнут"
    | _ -> "без отметки"

let private paintDay (today: DateOnly) (date: DateOnly) state (button: Button) =
    let isToday = date = today
    let isWeekend = date.DayOfWeek = DayOfWeek.Saturday || date.DayOfWeek = DayOfWeek.Sunday
    let background, border, foreground =
        match state with
        | Mark.Morning -> morning, morningLine, ink
        | Mark.Complete -> marked, markedLine, ink
        | _ when isToday -> cream, accent, accent
        | _ when date > today -> future, rule, futureInk
        | _ when isWeekend -> weekend, rule, ink
        | _ -> paper, rule, ink

    button.Background <- background
    button.BorderBrush <- if isToday then accent else border
    button.Foreground <- foreground

    let content = Grid(IsHitTestVisible = false)

    match state with
    | Mark.Morning ->
        content.Children.Add(Path(Data = halfDay, Fill = morningFill, Stretch = Stretch.Fill)) |> ignore
    | Mark.Complete ->
        content.Children.Add(Path(Data = fullDay, Stroke = crossed, StrokeThickness = 1.0,
                                  Stretch = Stretch.Fill, Margin = Thickness(3.0, 2.0, 3.0, 2.0))) |> ignore
    | _ -> ()

    content.Children.Add(TextBlock(Text = string date.Day, HorizontalAlignment = HorizontalAlignment.Center,
                                   VerticalAlignment = VerticalAlignment.Center)) |> ignore
    button.Content <- content

    let status = if date > today then "будущий день" else describeMark state
    let formatted = date.ToString("d MMMM yyyy, dddd", russian)
    let description = $"{formatted} · {status}"
    AutomationProperties.SetName(button, description)
    button.ToolTip <- description

let private editSettings (owner: Window) (year: int) journal =
    let dialog =
        Window(Owner = owner, Title = "Настройки календаря", WindowStartupLocation = WindowStartupLocation.CenterOwner,
               Width = 410.0, SizeToContent = SizeToContent.Height, ResizeMode = ResizeMode.NoResize,
               Background = owner.Background, Foreground = ink, FontFamily = owner.FontFamily, FontSize = 13.0,
               ShowInTaskbar = false, Topmost = owner.Topmost, Language = owner.Language)

    let panel = StackPanel(Margin = Thickness(22.0))
    let heading = TextBlock(Text = "Личная шкала времени", FontFamily = FontFamily("Georgia"), FontSize = 24.0,
                            Margin = Thickness(0.0, 0.0, 0.0, 14.0))
    panel.Children.Add heading |> ignore

    let yearInput title value =
        let label = TextBlock(Text = title, FontWeight = FontWeights.SemiBold)
        let box = TextBox(Text = value, Background = paper, BorderBrush = rule, MaxLength = 4,
                          Padding = Thickness(8.0, 5.0, 8.0, 5.0), Margin = Thickness(0.0, 4.0, 0.0, 12.0))
        AutomationProperties.SetLabeledBy(box, label)
        panel.Children.Add label |> ignore
        panel.Children.Add box |> ignore
        box

    let earliest = max 1 (year - 150)
    let latest = min 9999 (year + 150)
    let birth = yearInput "Год рождения" (journal.BirthYear |> Option.map string |> Option.defaultValue "")
    let horizon = yearInput "Последний год на шкале" (string (lastYear year journal))
    let message = TextBlock(Foreground = accent, TextWrapping = TextWrapping.Wrap)
    panel.Children.Add message |> ignore

    let actions = StackPanel(Orientation = Orientation.Horizontal, HorizontalAlignment = HorizontalAlignment.Right,
                             Margin = Thickness(0.0, 12.0, 0.0, 0.0))
    let cancel = Button(Content = "Отмена", IsCancel = true, Style = style owner "Quiet", Margin = Thickness(0.0, 0.0, 8.0, 0.0))
    let accept = Button(Content = "Сохранить", IsDefault = true, Style = style owner "Primary")
    actions.Children.Add cancel |> ignore
    actions.Children.Add accept |> ignore
    panel.Children.Add actions |> ignore
    dialog.Content <- panel

    let mutable result = journal

    accept.Click.Add(fun _ ->
        let birthText = birth.Text.Trim()
        let birthOk, birthYear = Int32.TryParse birthText
        let horizonOk, horizonYear = Int32.TryParse horizon.Text

        if birthText <> "" && (not birthOk || birthYear < earliest || birthYear > year) then
            message.Text <- $"Укажите год рождения от {earliest} до {year}."
            birth.Focus() |> ignore
        elif not horizonOk || horizonYear < year || horizonYear > latest then
            message.Text <- $"Укажите последний год от {year} до {latest}."
            horizon.Focus() |> ignore
        else
            result <-
                { journal with
                    BirthYear = if birthText = "" then None else Some birthYear
                    HorizonYear = if horizonYear = lastYear year journal then journal.HorizonYear else Some horizonYear }

            dialog.DialogResult <- Nullable true)

    dialog.Loaded.Add(fun _ ->
        birth.Focus() |> ignore
        birth.SelectAll())

    if dialog.ShowDialog().GetValueOrDefault() then Some result else None

let private bindWindow (window: Window) path initial =
    let button = control<Button> window
    let label = control<TextBlock> window
    let daysGrid = control<Grid> window "DaysGrid"
    let goalsBox = control<TextBox> window "Goals"
    let saveStatus = label "SaveStatus"
    let pin = button "Pin"
    let previous = button "PrevYear"
    let next = button "NextYear"
    let thisYear = button "ThisYear"
    let markToday = button "MarkToday"
    let clearToday = button "ClearToday"
    let dayButtons = Dictionary<DateOnly, Button>()
    let saveTimer = DispatcherTimer(Interval = TimeSpan.FromMilliseconds 550.0)
    let dateTimer = DispatcherTimer(Interval = TimeSpan.FromSeconds 20.0)
    let mutable today = calendarDate ()
    let mutable selectedYear = today.Year
    let mutable journal = initial
    let mutable dirty = false
    let mutable loadingGoals = false
    let mutable saveError = ""

    let flush () =
        saveTimer.Stop()

        if dirty then
            try
                save path journal
                dirty <- false
                saveError <- ""
                saveStatus.Text <- "Сохранено на этом компьютере"
                saveStatus.Foreground <- muted
                saveStatus.ToolTip <- null
            with
            | :? IOException as error ->
                saveError <- error.Message
            | :? UnauthorizedAccessException as error ->
                saveError <- error.Message

            if dirty then
                saveStatus.Text <- "Не удалось сохранить изменения"
                saveStatus.Foreground <- accent
                saveStatus.ToolTip <- saveError

        not dirty

    let updateJournal updated immediate =
        if updated <> journal then
            journal <- updated
            dirty <- true
            saveTimer.Stop()

            if immediate then flush () |> ignore
            else saveTimer.Start()

    let refreshPin () =
        window.Topmost <- journal.KeepOnTop
        pin.Content <- if journal.KeepOnTop then "✓  Поверх окон" else "Поверх окон"
        pin.Background <- if journal.KeepOnTop then cream else paper
        pin.ToolTip <- if journal.KeepOnTop then "Открепить окно" else "Держать календарь поверх других окон"
        AutomationProperties.SetName(pin, string pin.ToolTip)

    let refreshDayCount () =
        let count =
            journal.Days
            |> Seq.sumBy (fun pair -> if pair.Key.Year = selectedYear && pair.Value = Mark.Complete then 1 else 0)

        let total = if DateTime.IsLeapYear selectedYear then 366 else 365
        (label "DayCount").Text <- $"Зачёркнуто {count} из {total} дней"

    let refreshToday () =
        let state = mark today journal
        (label "TodayDate").Text <- today.ToString("d MMMM", russian)
        let weekday = today.ToString("dddd", russian)
        (label "TodayWeekday").Text <- russian.TextInfo.ToTitleCase weekday
        (label "MorningIcon").Text <- if state = Mark.Empty then "01" else "✓"
        (label "EveningIcon").Text <- if state = Mark.Complete then "✓" else "02"
        markToday.Content <-
            match state with
            | Mark.Morning -> "Отметить вечер"
            | Mark.Complete -> "День зачёркнут"
            | _ -> "Отметить утро"

        markToday.IsEnabled <- state <> Mark.Complete
        clearToday.Visibility <- if state = Mark.Empty then Visibility.Hidden else Visibility.Visible

    let updateDay date clear =
        updateJournal (changeDay today date clear journal) true

        match dayButtons.TryGetValue date with
        | true, day -> paintDay today date (mark date journal) day
        | _ -> ()

        refreshDayCount ()
        refreshToday ()

    let addCell row column (element: UIElement) =
        Grid.SetRow(element, row)
        Grid.SetColumn(element, column)
        daysGrid.Children.Add element |> ignore

    let renderDays () =
        daysGrid.Children.Clear()
        daysGrid.RowDefinitions.Clear()
        daysGrid.ColumnDefinitions.Clear()
        dayButtons.Clear()

        daysGrid.ColumnDefinitions.Add(ColumnDefinition(Width = GridLength 43.0))

        for day in 1 .. 31 do
            daysGrid.ColumnDefinitions.Add(ColumnDefinition())

        for row in 0 .. 12 do
            daysGrid.RowDefinitions.Add(RowDefinition(Height = GridLength.Auto))

        for day in 1 .. 31 do
            let text = TextBlock(Text = day.ToString("00"), Foreground = muted, FontFamily = FontFamily("Consolas"),
                                 FontSize = 10.0, HorizontalAlignment = HorizontalAlignment.Center,
                                 Margin = Thickness(0.0, 0.0, 0.0, 3.0))
            addCell 0 day text

        let cellStyle = style window "Cell"

        for month in 1 .. 12 do
            let text = TextBlock(Text = monthNames[month - 1], Style = style window "Caption",
                                 VerticalAlignment = VerticalAlignment.Center, Margin = Thickness(3.0, 0.0, 0.0, 0.0))
            addCell month 0 text

            let monthLength = DateTime.DaysInMonth(selectedYear, month)

            for day in 1 .. monthLength do
                let date = DateOnly(selectedYear, month, day)
                let cell = Button(Style = cellStyle, IsEnabled = (date <= today), IsTabStop = (date = today))
                ToolTipService.SetShowOnDisabled(cell, true)
                paintDay today date (mark date journal) cell
                dayButtons.Add(date, cell)
                cell.Click.Add(fun _ -> updateDay date false)
                cell.MouseRightButtonUp.Add(fun event ->
                    event.Handled <- true
                    updateDay date true)
                addCell month day cell

    let refreshPlaceholder () =
        (label "GoalsPlaceholder").Visibility <- visible (String.IsNullOrEmpty goalsBox.Text)

    let renderYear () =
        (label "YearHeading").Text <- string selectedYear
        (label "YearLabel").Text <-
            if selectedYear = today.Year then "ТЕКУЩИЙ ГОД"
            elif selectedYear < today.Year then "ПРОШЕДШИЙ ГОД"
            else "БУДУЩИЙ ГОД"

        thisYear.IsEnabled <- selectedYear <> today.Year
        previous.IsEnabled <- selectedYear > firstYear today.Year journal
        next.IsEnabled <- selectedYear < lastYear today.Year journal
        refreshDayCount ()
        renderDays ()

        loadingGoals <- true
        (label "GoalsHeading").Text <- $"Цели на {selectedYear} год"
        goalsBox.Text <- goals selectedYear journal
        loadingGoals <- false
        refreshPlaceholder ()

    let paintPastYear year (chip: Button) =
        let isMarked = Set.contains year journal.Years
        let text = TextBlock(Text = string year)

        if isMarked then
            text.TextDecorations <- TextDecorations.Strikethrough

        chip.Content <- text
        chip.Background <- if isMarked then marked else paper
        chip.BorderBrush <- if isMarked then markedLine else rule
        let description = if isMarked then "снять отметку" else "зачеркнуть год"
        chip.ToolTip <- $"{year} · {description}"
        AutomationProperties.SetName(chip, string chip.ToolTip)

    let renderPastYears () =
        let panel = control<WrapPanel> window "PastYears"
        panel.Children.Clear()
        let birth = min today.Year (defaultArg journal.BirthYear today.Year)
        let countLabel = label "PastCount"
        (control<DockPanel> window "BirthPrompt").Visibility <- visible journal.BirthYear.IsNone

        let updateCount () =
            let count = journal.Years |> Seq.filter (fun year -> year >= birth && year < today.Year) |> Seq.length
            countLabel.Text <- if journal.BirthYear.IsSome then $"ЗАЧЁРКНУТО {count} ИЗ {today.Year - birth}" else ""

        for year in birth .. today.Year - 1 do
            let chip = Button(Style = style window "YearChip")
            paintPastYear year chip
            chip.Click.Add(fun _ ->
                updateJournal (toggleYear today.Year year journal) true
                paintPastYear year chip
                updateCount ())
            panel.Children.Add chip |> ignore

        updateCount ()

    let selectYear year =
        if selectedYear <> year then
            selectedYear <- year
            renderYear ()

    let renderFutureYears () =
        let panel = control<WrapPanel> window "FutureYears"
        panel.Children.Clear()
        let horizon = lastYear today.Year journal
        (label "FutureRange").Text <- if horizon > today.Year then $"{today.Year + 1}–{horizon}" else ""
        (label "NoFuture").Visibility <- visible (horizon = today.Year)

        for year in today.Year + 1 .. horizon do
            let chip = Button(Content = string year, Style = style window "YearChip",
                              ToolTip = $"Открыть календарь на {year} год")
            chip.Click.Add(fun _ -> selectYear year)
            panel.Children.Add chip |> ignore

    let redraw () =
        selectedYear <- Math.Clamp(selectedYear, firstYear today.Year journal, lastYear today.Year journal)
        renderPastYears ()
        renderYear ()
        renderFutureYears ()
        refreshToday ()

    let refreshDate () =
        let current = calendarDate ()

        if current <> today then
            if selectedYear = today.Year then selectedYear <- current.Year
            today <- current
            redraw ()

    let showSettings () =
        match editSettings window today.Year journal with
        | Some updated ->
            updateJournal updated true
            redraw ()
        | None -> ()

    saveTimer.Tick.Add(fun _ -> flush () |> ignore)
    dateTimer.Tick.Add(fun _ -> refreshDate ())
    window.Activated.Add(fun _ -> refreshDate ())
    pin.Click.Add(fun _ ->
        updateJournal { journal with KeepOnTop = not journal.KeepOnTop } true
        refreshPin ())
    (button "Settings").Click.Add(fun _ -> showSettings ())
    (button "SetBirth").Click.Add(fun _ -> showSettings ())
    previous.Click.Add(fun _ -> selectYear (selectedYear - 1))
    next.Click.Add(fun _ -> selectYear (selectedYear + 1))
    thisYear.Click.Add(fun _ -> selectYear today.Year)
    markToday.Click.Add(fun _ -> updateDay today false)
    clearToday.Click.Add(fun _ -> updateDay today true)
    goalsBox.TextChanged.Add(fun _ ->
        if not loadingGoals then
            refreshPlaceholder ()
            updateJournal (changeGoals selectedYear goalsBox.Text journal) false)
    window.Closing.Add(fun event ->
        if not (flush ()) then
            let result =
                MessageBox.Show(window, $"Изменения не сохранились. Закрыть календарь?\n\n{saveError}",
                                "Календарик-пинарик", MessageBoxButton.YesNo, MessageBoxImage.Warning, MessageBoxResult.No)
            event.Cancel <- result <> MessageBoxResult.Yes)
    window.Closed.Add(fun _ -> dateTimer.Stop())

    refreshPin ()
    redraw ()
    dateTimer.Start()

let private run () =
    let directory = System.IO.Path.Combine(Environment.GetFolderPath(Environment.SpecialFolder.LocalApplicationData), "RnxKickCalendar")
    Directory.CreateDirectory directory |> ignore

    use _lease =
        try
            File.Open(System.IO.Path.Combine(directory, "calendar.lock"), FileMode.OpenOrCreate, FileAccess.ReadWrite, FileShare.None)
        with
        | :? IOException as error when error.HResult = -2147024864 ->
            raise (InvalidOperationException("Календарь уже открыт.", error))

    let path = System.IO.Path.Combine(directory, "calendar.json")
    let initial = load path
    let app = Application()
    use markup = Assembly.GetExecutingAssembly().GetManifestResourceStream "Window.xaml"
    let window = XamlReader.Load markup :?> Window
    window.Width <- min window.Width SystemParameters.WorkArea.Width
    window.Height <- min window.Height SystemParameters.WorkArea.Height
    bindWindow window path initial
    app.Run window

[<STAThread; EntryPoint>]
let main _ =
    try run ()
    with error ->
        MessageBox.Show($"Не удалось открыть календарь.\n\n{error.Message}", "Календарик-пинарик",
                        MessageBoxButton.OK, MessageBoxImage.Error) |> ignore
        1
