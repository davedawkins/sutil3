module Examples

open Sutil.Html
open Types

let allExamples =
    [
        {
            Pass = true
            Category = "Introduction"
            Title = "Hello World"
            Link =
                AppLink(
                    HelloWorld.view,
                    [
                        "Introduction/HelloWorld.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Introduction"
            Title = "Dynamic attributes"
            Link =
                AppLink(
                    DynamicAttributes.view,
                    [
                        "Introduction/DynamicAttributes.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Introduction"
            Title = "Styling"
            Link =
                AppLink(
                    StylingExample.view,
                    [
                        "Introduction/Styling.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Introduction"
            Title = "Nested components"
            Link =
                AppLink(
                    NestedComponents.view,
                    [
                        "Introduction/NestedComponents.fs"
                        "Introduction/Nested.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Introduction"
            Title = "HTML tags"
            Link =
                AppLink(
                    HtmlTags.view,
                    [
                        "Introduction/HtmlTags.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Reactivity"
            Title = "Reactive assignments"
            Link =
                AppLink(
                    Counter.view,
                    [
                        "Reactivity/Counter.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Reactivity"
            Title = "Reactive declarations"
            Link =
                AppLink(
                    ReactiveDeclarations.view,
                    [
                        "Reactivity/ReactiveDeclarations.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Reactivity"
            Title = "Reactive statements"
            Link =
                AppLink(
                    ReactiveStatements.view,
                    [
                        "Reactivity/ReactiveStatements.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Logic"
            Title = "If blocks"
            Link =
                AppLink(
                    LogicIf.view,
                    [
                        "Logic/If.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Logic"
            Title = "Else blocks"
            Link =
                AppLink(
                    LogicElse.view,
                    [
                        "Logic/Else.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Logic"
            Title = "Else-if blocks"
            Link =
                AppLink(
                    LogicElseIf.view,
                    [
                        "Logic/ElseIf.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Logic"
            Title = "Static each blocks"
            Link =
                AppLink(
                    StaticEachBlocks.view,
                    [
                        "Logic/StaticEach.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Logic"
            Title = "Static each with index"
            Link =
                AppLink(
                    StaticEachWithIndex.view,
                    [
                        "Logic/StaticEachWithIndex.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Logic"
            Title = "Each blocks"
            Link =
                AppLink(
                    EachBlocks.view,
                    [
                        "Logic/EachBlocks.fs"
                    ]
                )
        }
        {
            Pass = false
            Category = "Logic"
            Title = "Keyed-each blocks"
            Link =
                AppLink(
                    KeyedEachBlocks.view,
                    [
                        "Logic/KeyedEachBlocks.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Logic"
            Title = "Await blocks"
            Link =
                AppLink(
                    AwaitBlocks.view,
                    [
                        "Logic/AwaitBlocks.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Events"
            Title = "DOM events"
            Link =
                AppLink(
                    DomEvents.view,
                    [
                        "Events/Dom.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Events"
            Title = "Custom events"
            Link =
                AppLink(
                    CustomEvents.view,
                    [
                        "Events/Custom.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Events"
            Title = "Event modifiers"
            Link =
                AppLink(
                    EventModifiers.view,
                    [
                        "Events/Modifiers.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Transitions"
            Title = "Transition"
            Link =
                AppLink(
                    Transition.view,
                    [
                        "Transitions/Simple.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Transitions"
            Title = "Adding parameters"
            Link =
                AppLink(
                    TransitionParameters.view,
                    [
                        "Transitions/WithParameters.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Transitions"
            Title = "In and out"
            Link =
                AppLink(
                    TransitionInOut.view,
                    [
                        "Transitions/InOut.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Transitions"
            Title = "Custom CSS"
            Link =
                AppLink(
                    TransitionCustomCss.view,
                    [
                        "Transitions/CustomCss.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Transitions"
            Title = "Custom Code"
            Link =
                AppLink(
                    TransitionCustom.view,
                    [
                        "Transitions/CustomCode.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Transitions"
            Title = "Transition events"
            Link =
                AppLink(
                    TransitionEvents.view,
                    [
                        "Transitions/Events.fs"
                    ]
                )
        }
        {
            Pass = false
            Category = "Transitions"
            Title = "Animation"
            Link =
                AppLink(
                    Todos.view,
                    [
                        "Transitions/Todos.fs"
                    ]
                )
        }

        // Needs Bulma
        {
            Pass = true
            Category = "Bindings"
            Title = "Text inputs"
            Link =
                AppLink(
                    TextInputs.view,
                    [
                        "Bindings/TextInputs.fs"
                    ]
                )
        }

        // Needs Bulma
        {
            Pass = true
            Category = "Bindings"
            Title = "Numeric inputs"
            Link =
                AppLink(
                    NumericInputs.view,
                    [
                        "Bindings/NumericInputs.fs"
                    ]
                )
        }

        {
            Pass = true
            Category = "Bindings"
            Title = "Checkbox inputs"
            Link =
                AppLink(
                    CheckboxInputs.view,
                    [
                        "Bindings/CheckboxInputs.fs"
                    ]
                )
        }

        // Needs Bulma
        {
            Pass = true
            Category = "Bindings"
            Title = "Group inputs"
            Link =
                AppLink(
                    GroupInputs.view,
                    [
                        "Bindings/GroupInputs.fs"
                    ]
                )
        }

        // Needs Bulma
        {
            Pass = true
            Category = "Bindings"
            Title = "Textarea inputs"
            Link =
                AppLink(
                    TextArea.view,
                    [
                        "Bindings/TextArea.fs"
                    ]
                )
        }

        // Needs Bulma
        {
            Pass = true
            Category = "Bindings"
            Title = "File inputs"
            Link =
                AppLink(
                    FileInputs.view,
                    [
                        "Bindings/FileInputs.fs"
                    ]
                )
        }

        {
            Pass = true
            Category = "Bindings"
            Title = "Select bindings"
            Link =
                AppLink(
                    SelectBindings.view,
                    [
                        "Bindings/SelectBindings.fs"
                    ]
                )
        }

        // Needs Bulma
        {
            Pass = true
            Category = "Bindings"
            Title = "Select multiple"
            Link =
                AppLink(
                    SelectMultiple.view,
                    [
                        "Bindings/SelectMultiple.fs"
                    ]
                )
        }

        {
            Pass = true
            Category = "Bindings"
            Title = "Dimensions"
            Link =
                AppLink(
                    Dimensions.view,
                    [
                        "Bindings/Dimensions.fs"
                    ]
                )
        }

        {
            Pass = true
            Category = "Svg"
            Title = "Bar chart"
            Link =
                AppLink(
                    BarChart.view,
                    [
                        "Svg/BarChart.fs"
                    ]
                )
        }

        {
            Pass = true
            Category = "Miscellaneous"
            Title = "Spreadsheet"
            Link =
                AppLink(
                    Spreadsheet.App.view,
                    [
                        "Miscellaneous/Spreadsheet/Spreadsheet.fs"
                        "Miscellaneous/Spreadsheet/Evaluator.fs"
                        "Miscellaneous/Spreadsheet/Parser.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Miscellaneous"
            Title = "Modal"
            Link =
                AppLink(
                    Modal.view,
                    [
                        "Miscellaneous/Modal.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Miscellaneous"
            Title = "Login"
            Link =
                AppLink(
                    LoginExample.view,
                    [
                        "Miscellaneous/LoginExample.fs"
                        "Miscellaneous/Login.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Miscellaneous"
            Title = "Drag-sortable list"
            Link =
                AppLink(
                    SortableTimerList.view,
                    [
                        "Miscellaneous/SortableTimerList.fs"
                        "Miscellaneous/DragDropListSort.fs"
                        "Miscellaneous/TimerWithButton.fs"
                        "Miscellaneous/TimerLogic.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Miscellaneous"
            Title = "SAFE client"
            Link =
                AppLink(
                    SAFE.view,
                    [
                        "Miscellaneous/SafeClient.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Miscellaneous"
            Title = "Data Simulation"
            Link =
                AppLink(
                    DataSim.view,
                    [
                        "Miscellaneous/DataSim.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "Miscellaneous"
            Title = "Web Components"
            Link =
                AppLink(
                    WebComponents.view,
                    [
                        "Miscellaneous/WebComponents.fs"
                    ]
                )
        }

        // Not in Sutil 2
        // { Pass =false; Category = "Miscellaneous";   Title = "Draw";  Link = AppLink (Draw.view , ["Draw.fs"]) }
        // { Pass =false; Category = "Miscellaneous";   Title = "Fragment";  Link = AppLink (Fragment.view , ["Fragment.fs"]) }

        {
            Pass = true
            Category = "7Guis"
            Title = "Cells"
            Link =
                AppLink(
                    SevenGuisCells.view,
                    [
                        "7Guis/Cells.fs"
                    ]
                )
        }
        {
            Pass = true
            Category = "7Guis"
            Title = "CRUD"
            Link =
                AppLink(
                    CRUD.view,
                    [
                        "7Guis/CRUD.fs"
                    ]
                )
        }
    ]
