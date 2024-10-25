module Examples

open Sutil.Html
open Types

let CompilationFailure() =
    Html.div "Compilation failure :-("

// module TransitionParameters = let view = CompilationFailure
// module TransitionInOut = let view = CompilationFailure
// module TransitionCustomCss = let view = CompilationFailure
// module TransitionCustom = let view = CompilationFailure
// module TransitionEvents = let view = CompilationFailure

#if !FANTOMAS
let allExamples = [
        { Pass = true; Category = "Introduction";Title = "Hello World";  Link = AppLink (HelloWorld.view , ["HelloWorld.fs"])}
        { Pass = true; Category = "Introduction";Title = "Dynamic attributes";  Link = AppLink (DynamicAttributes.view , ["DynamicAttributes.fs"])}
        { Pass = true; Category = "Introduction";Title = "Styling";  Link = AppLink (StylingExample.view , ["Styling.fs"])}
        { Pass = true; Category = "Introduction";Title = "Nested components";  Link = AppLink (NestedComponents.view , ["NestedComponents.fs"; "Nested.fs"])}
        { Pass = true; Category = "Introduction";Title = "HTML tags";  Link = AppLink (HtmlTags.view , ["HtmlTags.fs"])}
        { Pass = true; Category = "Reactivity";  Title = "Reactive assignments";  Link = AppLink (Counter.view , ["Counter.fs"])}
        { Pass = true; Category = "Reactivity";  Title = "Reactive declarations";  Link = AppLink (ReactiveDeclarations.view , ["ReactiveDeclarations.fs"]) }
        { Pass = true; Category = "Reactivity";  Title = "Reactive statements";  Link = AppLink (ReactiveStatements.view , ["ReactiveStatements.fs"]) }
        { Pass = true; Category = "Logic"; Title = "If blocks"; Link = AppLink (LogicIf.view, ["LogicIf.fs"])  }
        { Pass = true; Category = "Logic"; Title = "Else blocks"; Link = AppLink (LogicElse.view, ["LogicElse.fs"])  }
        { Pass = true; Category = "Logic"; Title = "Else-if blocks"; Link = AppLink (LogicElseIf.view, ["LogicElseIf.fs"])  }
        { Pass = true; Category = "Logic"; Title = "Static each blocks"; Link = AppLink (StaticEachBlocks.view, ["StaticEach.fs"])  }
        { Pass = true; Category = "Logic"; Title = "Static each with index"; Link = AppLink (StaticEachWithIndex.view, ["StaticEachWithIndex.fs"])  }
        { Pass = true; Category = "Logic"; Title = "Each blocks"; Link = AppLink (EachBlocks.view, ["EachBlocks.fs"])  }
        { Pass = false; Category = "Logic"; Title = "Keyed-each blocks"; Link = AppLink (KeyedEachBlocks.view, ["KeyedEachBlocks.fs"])  }
        { Pass = true; Category = "Logic"; Title = "Await blocks"; Link = AppLink (AwaitBlocks.view, ["AwaitBlocks.fs"])  }
        { Pass = true; Category = "Events"; Title = "DOM events"; Link = AppLink (DomEvents.view, ["DomEvents.fs"])  }
        { Pass = true; Category = "Events"; Title = "Custom events"; Link = AppLink (CustomEvents.view, ["CustomEvents.fs"])  }
        { Pass = true; Category = "Events"; Title = "Event modifiers"; Link = AppLink (EventModifiers.view, ["EventModifiers.fs"])  }
        { Pass =true; Category = "Transitions"; Title = "Transition"; Link = AppLink (Transition.view, ["Transition.fs"])  }
        { Pass =true; Category = "Transitions"; Title = "Adding parameters"; Link = AppLink (TransitionParameters.view, ["TransitionParameters.fs"])  }
        { Pass =true; Category = "Transitions"; Title = "In and out"; Link = AppLink (TransitionInOut.view, ["TransitionInOut.fs"])  }
        { Pass =true; Category = "Transitions"; Title = "Custom CSS"; Link = AppLink (TransitionCustomCss.view, ["TransitionCustomCss.fs"])  }
        { Pass =true; Category = "Transitions"; Title = "Custom Code"; Link = AppLink (TransitionCustom.view, ["TransitionCustom.fs"])  }
        { Pass =true; Category = "Transitions"; Title = "Transition events"; Link = AppLink (TransitionEvents.view, ["TransitionEvents.fs"])  }
        { Pass =false; Category = "Transitions"; Title = "Animation"; Link = AppLink (Todos.view, ["Todos.fs"])  }

        // Needs Bulma
        { Pass =true; Category = "Bindings";   Title = "Text inputs";  Link = AppLink (TextInputs.view , ["TextInputs.fs"]) }

        // Needs Bulma
        { Pass =true; Category = "Bindings";   Title = "Numeric inputs";  Link = AppLink (NumericInputs.view , ["NumericInputs.fs"]) }

        { Pass =true; Category = "Bindings";   Title = "Checkbox inputs";  Link = AppLink (CheckboxInputs.view , ["CheckboxInputs.fs"]) }

        // Needs Bulma
        { Pass =true; Category = "Bindings";   Title = "Group inputs";  Link = AppLink (GroupInputs.view , ["GroupInputs.fs"]) }

        // Needs Bulma
        { Pass =true; Category = "Bindings";   Title = "Textarea inputs";  Link = AppLink (TextArea.view , ["TextArea.fs"]) }
        
        // Needs Bulma
        { Pass =true; Category = "Bindings";   Title = "File inputs";  Link = AppLink (FileInputs.view , ["FileInputs.fs"]) }

        { Pass =true; Category = "Bindings";   Title = "Select bindings";  Link = AppLink (SelectBindings.view , ["SelectBindings.fs"]) }

        // Needs Bulma
        { Pass =true; Category = "Bindings";   Title = "Select multiple";  Link = AppLink (SelectMultiple.view , ["SelectMultiple.fs"]) }


        { Pass =true; Category = "Bindings";   Title = "Dimensions";  Link = AppLink (Dimensions.view , ["Dimensions.fs"]) }

        { Pass =true; Category = "Svg";   Title = "Bar chart";  Link = AppLink (BarChart.view , ["BarChart.fs"]) }

        { Pass =true; Category = "Miscellaneous";   Title = "Spreadsheet";  Link = AppLink (Spreadsheet.view , ["Spreadsheet.fs"; "Evaluator.fs"; "Parser.fs"]) }
        { Pass =true; Category = "Miscellaneous";   Title = "Modal";  Link = AppLink (Modal.view , ["Modal.fs"]) }
        { Pass =true; Category = "Miscellaneous";   Title = "Login";  Link = AppLink (LoginExample.view , ["LoginExample.fs"; "Login.fs"]) }
        { Pass =true; Category = "Miscellaneous";   Title = "Drag-sortable list";  Link = AppLink (SortableTimerList.view , ["SortableTimerList.fs"; "DragDropListSort.fs"; "TimerWithButton.fs"; "TimerLogic.fs"]) }
        { Pass =true; Category = "Miscellaneous";   Title = "SAFE client";  Link = AppLink (SAFE.view , ["SafeClient.fs"]) }
        { Pass =true; Category = "Miscellaneous";   Title = "Data Simulation";  Link = AppLink (DataSim.view , ["DataSim.fs"]) }
        { Pass =true; Category = "Miscellaneous";   Title = "Web Components";  Link = AppLink (WebComponents.view , ["WebComponents.fs"]) }

        // Not in Sutil 2
        // { Pass =false; Category = "Miscellaneous";   Title = "Draw";  Link = AppLink (Draw.view , ["Draw.fs"]) }
        // { Pass =false; Category = "Miscellaneous";   Title = "Fragment";  Link = AppLink (Fragment.view , ["Fragment.fs"]) }

        { Pass =true; Category = "7Guis";   Title = "Cells";  Link = AppLink (SevenGuisCells.view , ["Cells.fs"]) }
        { Pass =true; Category = "7Guis";   Title = "CRUD";  Link = AppLink (CRUD.view , ["CRUD.fs"]) }
    ]
#endif
