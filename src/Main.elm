import Html exposing (Html, button, div, text, input, h1, p, strong, br)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (class, placeholder)
import String exposing (contains)

type Msg = Search String
         | Select Code

type alias Code =
  { process : CodePart
  , modifiers : List CodePart
  }

type alias CodePart =
  { name : String
  , note : Maybe String
  }

type alias Model =
  { selected : Maybe Code
  , search : String
  }

update : Msg -> Model -> Model
update msg model =
  case msg of
    Search search ->
      { model | search = search }
    Select code ->
      { model | selected = Just code }


view : Model -> Html Msg
view m =
  let
    codes : List Code
    codes =
      allCodes
        |> List.sortBy (String.toLower << .name << .process)
        |> List.filter (showCode m.search)

    renderCode : Code -> Html Msg
    renderCode c =
      let
        highlighted = if (m.search /= "") && (contains m.search c.process.name) then " hl" else ""
        selected =
          case m.selected of
            Just selected ->
              if selected.process.name == c.process.name then " selected" else ""
            Nothing ->
              ""

      in
        div [ class ("code" ++ selected), onClick <| Select c ]
          [ h1 [ class ("process" ++ highlighted) ] [ text c.process.name ]
          , div [ class "modifiers"] <| List.map (renderMod m.search) c.modifiers
          ]

  in
    div [ class "codebook" ]
      [ div [ class "panel" ]
         [ input [ placeholder "search" , onInput Search ] [] 
         , div [ class "codes" ] <| List.map (renderCode) codes
         ]
      , renderSelection m.selected
      ]

showCode : String -> Code -> Bool
showCode search code =
  let
    nameMatches =
      contains (String.toLower search) << String.toLower << .name 
  in
    if search == ""
    then True
    else (nameMatches code.process || List.any nameMatches code.modifiers)



renderMod : String -> CodePart -> Html Msg
renderMod search m =
  let
    klass = 
      if (search /= "") && (contains search m.name)
      then "modifier hl" 
      else "modifier"
  in
    div [ class klass ]
      [ text ("+ " ++ m.name)
      ]

renderSelection : Maybe Code -> Html Msg
renderSelection mcode =
  let
      blankState =
        div [ class "selection" ] []

      renderCode code =
        div [ class "selection" ] 
          [ h1 [] [ text code.process.name ]
          , p [] [ text <| Maybe.withDefault "" code.process.note ]
          , div [] <| List.map renderModifier code.modifiers 
          ]

      renderModifier mod =
         p []
           [ strong [] [ text ("+  " ++ mod.name) ]
           , br [] []
           , text <| Maybe.withDefault "" mod.note
           ]

  in
    mcode
      |> Maybe.map renderCode
      |> Maybe.withDefault blankState


initialModel : Model
initialModel =
  { selected = Nothing 
  , search = ""
  }

main =
  Html.beginnerProgram { model = initialModel, view = view, update = update }

allCodes : List Code
allCodes =
  [ { process = { name = "being frustrated", note = Nothing }
    , modifiers =
        [ { name = "speed", note = Nothing }
        , { name = "cost", note = Nothing }
        ]
    }
  , { process = { name = "being inspired", note = Just "ie. by the startup dream" }
    , modifiers = []
    }
  , { process = { name = "being referred/introduced", note = Nothing }
    , modifiers =
        [ { name = "to lawyer", note = Nothing }
        , { name = "by accelerator", note = Nothing }
        , { name = "to advisors", note = Nothing }
        , { name = "by investors ", note = Nothing }
        , { name = "to accountant", note = Nothing }
        , { name = "to clients/customers", note = Nothing }
        ]
    }
  , { process = { name = "being slowed down", note = Nothing }
    , modifiers = []
    }
  , { process = { name = "building a product", note = Just "something you will sell" }
    , modifiers =
        [ { name = "mvp", note = Nothing }
        , { name = "prototype", note = Nothing }
        ]
    }
  , { process = { name = "building demand", note = Nothing }
    , modifiers =
        [ { name = "with content", note = Nothing }
        , { name = "with thought leadership", note = Nothing }
        ]
    }
  , { process = { name = "checking a lawyer's work", note = Nothing }
    , modifiers = []
    }
  , { process = { name = "creating an option pool", note = Nothing }
    , modifiers =
        [ { name = "at fundraising", note = Nothing }
        ]
    }
  , { process = { name = "DIY", note = Just "Emphasis on that they are doing it themselves (the mentality), not on what they are doing" }
    , modifiers =
        [ { name = "being able to do it", note = Nothing }
        , { name = "good fit", note = Nothing }
        , { name = "expense tracking", note = Just "use when they don't mention their tools. for example, 'i was holding myself accountable.' cf. using expense tracking + diy" }
        ]
    }
  , { process = { name = "exiting", note = Nothing }
    , modifiers =
        [ { name = "acquisition", note = Nothing }
        ]
    }
  , { process = { name = "failing", note = Nothing }
    , modifiers = []
    }
  , { process = { name = "finding office space", note = Nothing }
    , modifiers = 
      [ { name = "co-working", note = Nothing }
      ]
    }
  , { process = { name = "finishing school", note = Nothing }
    , modifiers = []
    }
  , { process = { name = "firing", note = Just "doesn't matter whether they're contractors or employees" }
    , modifiers = []
    }
  , { process = { name = "fixing a mistake", note = Nothing }
    , modifiers =
        [ { name = "expensive", note = Nothing }
        , { name = "regarding incorporation", note = Nothing }
        ]
    }
  , { process = { name = "fundraising", note = Nothing }
    , modifiers =
        [ { name = "seed", note = Nothing }
        , { name = "friends & family", note = Nothing }
        , { name = "from accelerator", note = Nothing }
        , { name = "from angel(s)", note = Nothing }
        , { name = "from VC", note = Nothing }
        , { name = "professional", note = Nothing }
        ]
    }
  , { process = { name = "funding", note = Just "not a round, may include resources" }
    , modifiers =
        [ { name = "self", note = Nothing }
        , { name = "from an award", note = Nothing }
        , { name = "from incubator", note = Nothing }
        ]
    }
  , { process = { name = "getting advice", note = Nothing }
    , modifiers =
        [ { name = "legal", note = Nothing }
        , { name = "early", note = Nothing }
        , { name = "for incorporation", note = Nothing }
        , { name = "from school", note = Nothing }
        , { name = "from lawyer", note = Nothing }
        , { name = "for option pool", note = Nothing }
        , { name = "from accelerator", note = Nothing }
        , { name = "not helpful", note = Nothing }
        , { name = "about fundraising", note = Nothing }
        , { name = "conflicting", note = Nothing }
        , { name = "on hiring", note = Nothing }
        , { name = "from investors", note = Nothing }
        , { name = "from network", note = Just "if you use this code, consider also coding \"using network\" to specify the type of network" }
        , { name = "PR", note = Nothing }
        ]
    }
  , { process = { name = "getting help", note = Nothing }
    , modifiers =
        [ { name = "legal", note = Nothing }
        , { name = "from school", note = Nothing }
        , { name = "from accelerator", note = Nothing }
        , { name = "with incorporation", note = Nothing }
        , { name = "from lawyer", note = Nothing }
        , { name = "documents", note = Nothing }
        , { name = "with expense tracking", note = Nothing }
        , { name = "from investors", note = Nothing }
        , { name = "from network", note = Just "if you use this code, consider also coding \"using network\" to specify the type of network" }
        , { name = "PR", note = Nothing }
        ]
    }
  , { process = { name = "getting traction", note = Nothing }
    , modifiers =
        [ { name = "quickly", note = Nothing }
        ]
    }
  , { process = { name = "getting a valuation", note = Nothing }
    , modifiers =
        [ { name = "hard", note = Nothing }
        ]
    }
  , { process = { name = "growing a company", note = Just "internal pressure: hiring, office, culture etc., as opposed to \"scaling\" which we use to mean dealing with external pressure (many customers)" }
    , modifiers =
        [ { name = "business plan", note = Nothing }
        , { name = "culture", note = Nothing }
        , { name = "slow", note = Nothing }
        , { name = "expensive", note = Nothing }
        ]
    }
  , { process = { name = "having a board", note = Nothing }
    , modifiers = []
    }
  , { process = { name = "having competitors", note = Nothing }
    , modifiers = []
    }
  , { process = { name = "having connections", note = Nothing }
    , modifiers =
        [ { name = "important", note = Nothing }
        , { name = "founders", note = Nothing }
        , { name = "through school", note = Nothing }
        ]
    }
  , { process = { name = "having a team", note = Nothing }
    , modifiers =
        [ { name = "good", note = Nothing }
        , { name = "bad", note = Nothing }
        ]
    }
  , { process = { name = "having an idea", note = Just "for a company/product" }
    , modifiers =
        [ { name = "good", note = Nothing }
        , { name = "bad", note = Nothing }
        , { name = "many", note = Nothing }
        , { name = "from experience", note = Nothing }
        ]
    }
  , { process = { name = "having experience", note = Nothing }
    , modifiers =
        [ { name = "in industry", note = Nothing }
        , { name = "with startups", note = Nothing }
        , { name = "with board relations", note = Nothing }
        , { name = "non-founder", note = Just "working at a startup" }
        , { name = "starting a company", note = Nothing }
        , { name = "technical", note = Nothing }
        , { name = "hiring", note = Nothing }
        ]
    }
  , { process = { name = "helping network", note = Nothing }
    , modifiers = []
    }
  , { process = { name = "hiring contractors", note = Just "this use of 'contractors' refers to specific people, as in bringing on a developer to the team but not paying them as an employee. if the 'contractor' is a firm, it's probably 'outsourcing' instead." }
    , modifiers =
        [ { name = "designers", note = Nothing }
        , { name = "developers", note = Nothing }
        , { name = "part-time", note = Nothing }
        , { name = "poor fit", note = Nothing }
        , { name = "specialized", note = Nothing }
        , { name = "hard", note = Nothing }
        ]
    }
  , { process = { name = "hiring counsel", note = Nothing }
    , modifiers =
        [ { name = "at incorporation", note = Nothing }
        , { name = "introduced", note = Nothing }
        , { name = "based on feels", note = Just "a personality, interest in project, good meeting, etc" }
        , { name = "based on their network", note = Just "the lawyer/firm's network" }
        , { name = "based on reputation", note = Nothing }
        , { name = "based on their experience with startups", note = Just "the lawyer/firm's experience" }
        ]
    }
  , { process = { name = "hiring employees", note = Nothing }
    , modifiers =
        [ { name = "full-time", note = Nothing }
        , { name = "first", note = Nothing }
        , { name = "multiple", note = Nothing }
        , { name = "smart people", note = Nothing }
        , { name = "designers", note = Nothing }
        , { name = "expensive", note = Nothing }
        , { name = "specialized", note = Nothing }
        , { name = "hard", note = Nothing }
        , { name = "engineers", note = Nothing }
        ]
    }
  , { process = { name = "hiring interns", note = Nothing }
    , modifiers =
        [ { name = "first", note = Nothing }
        ]
    }
  , { process = { name = "incorporating", note = Nothing }
    , modifiers =
        [ { name = "C-Corp", note = Nothing }
        , { name = "LLC", note = Nothing }
        , { name = "doing it yourself", note = Nothing }
        , { name = "expensive", note = Nothing }
        , { name = "initially", note = Nothing }
        , { name = "reincorporating", note = Nothing }
        , { name = "simply", note = Nothing }
        , { name = "lawyer", note = Nothing }
        ]
    }
  , { process = { name = "issuing options", note = Nothing }
    , modifiers =
        [ { name = "to employees", note = Nothing }
        , { name = "to founders", note = Nothing }
        , { name = "to advisors", note = Nothing }
        ]
    }
  , { process = { name = "issuing shares", note = Nothing }
    , modifiers =
        [ { name = "initial", note = Nothing }
        , { name = "for contributions", note = Just "other than investment" }
        ]
    }
  , { process = { name = "iterating", note = Nothing }
    , modifiers = []
    }
  , { process = { name = "joining an accelerator", note = Just "please add a subcategory tag for the name of the accelerator, if known" }
    , modifiers =
        [ { name = "AREA", note = Nothing }
        , { name = "early", note = Nothing }
        , { name = "ERA", note = Nothing }
        , { name = "TechStars", note = Nothing }
        ]
    }
  , { process = { name = "knowing (about)", note = Nothing }
    , modifiers =
        [ { name = "nothing", note = Nothing }
        , { name = "term sheet norms", note = Nothing }
        , { name = "company structure", note = Nothing }
        , { name = "hiring", note = Nothing }
        , { name = "Saas finances", note = Nothing }
        , { name = "board relations", note = Nothing }
        , { name = "taxes", note = Nothing }
        , { name = "PR", note = Nothing }
        , { name = "accounting", note = Nothing }
        ]
    }
  , { process = { name = "launching a product", note = Nothing }
    , modifiers = []
    }
  , { process = { name = "making a decision", note = Nothing }
    , modifiers =
        [ { name = "based on complexity", note = Nothing }
        , { name = "based on common wisdom", note = Nothing }
        , { name = "based on finances", note = Nothing }
        , { name = "based on legal obligations", note = Nothing }
        , { name = "based on taxes", note = Nothing }
        , { name = "about how to classify employees", note = Nothing }
        ]
    }
  , { process = { name = "making a mistake", note = Nothing }
    , modifiers =
        [ { name = "expensive", note = Nothing }
        , { name = "hiring", note = Nothing }
        , { name = "lawyer", note = Just "the lawyer is the mistake, not the lawyer made the mistake" }
        , { name = "accounting", note = Nothing }
        , { name = "early", note = Nothing }
        , { name = "legal", note = Nothing }
        , { name = "LLC", note = Nothing }
        , { name = "PEO", note = Nothing }
        , { name = "personal", note = Nothing }
        , { name = "vendors", note = Nothing }
        , { name = "not hiring", note = Just "should have hired, but didn't" }
        , { name = "culture", note = Nothing }
        , { name = "cap table management", note = Nothing }
        , { name = "focus", note = Nothing }
        , { name = "growth strategy", note = Nothing }
        ]
    }
  , { process = { name = "managing cash flow", note = Nothing }
    , modifiers = []
    }
  , { process = { name = "managing inventory", note = Nothing }
    , modifiers =
        [ { name = "hard", note = Nothing }
        ]
    }
  , { process = { name = "managing stakeholder relationships", note = Nothing }
    , modifiers =
        [ { name = "board", note = Nothing }
        , { name = "shareholders", note = Nothing }
        ]
     }
  , { process = { name = "meeting/finding co-founder(s)", note = Nothing }
    , modifiers =
        [ { name = "late", note = Nothing }
        , { name = "for fundraising", note = Nothing }
        ]
    }
  , { process = { name = "needing advice", note = Nothing }
    , modifiers =
        []
    }
  , { process = { name = "needing a bank account", note = Nothing }
    , modifiers =
        []
    }
  , { process = { name = "needing EIN", note = Nothing }
    , modifiers =
        []
    }
  , { process = { name = "needing expense tracking", note = Nothing }
    , modifiers =
        []
    }
  , { process = { name = "needing a lawyer", note = Nothing }
    , modifiers =
        []
    }
  , { process = { name = "needing office space", note = Nothing }
    , modifiers =
        [ { name = "co-working", note = Nothing }
        , { name = "expanding", note = Nothing }
        , { name = "lab space", note = Nothing }
        , { name = "manufacturing", note = Nothing }
        , { name = "location", note = Nothing }
        ]
    }
  , { process = { name = "needing Stripe", note = Nothing }
    , modifiers =
        []
    }
  , { process = { name = "needing a service", note = Just "as in, any service if not specified" }
    , modifiers =
        []
    }
  , { process = { name = "negotiating", note = Nothing }
    , modifiers =
        []
    }
  , { process = { name = "not tracking expenses", note = Just "ie. not doing anything" }
    , modifiers =
        []
    }
  , { process = { name = "offering benefits", note = Nothing }
    , modifiers =
        []
    }
  , { process = { name = "opening a bank account", note = Nothing }
    , modifiers =
        []
    }
  , { process = { name = "outsourcing", note = Nothing }
    , modifiers =
        [ { name = "CFO", note = Nothing }
        , { name = "bookkeeper", note = Nothing }
        , { name = "finances", note = Nothing }
        , { name = "free", note = Nothing }
        , { name = "because of money", note = Nothing }
        ]
    }
  , { process = { name = "paying people", note = Nothing }
    , modifiers =
        [ { name = "employees", note = Nothing }
        , { name = "founders/selves", note = Nothing }
        , { name = "under-market", note = Nothing }
        ]
    }
  , { process = { name = "pivoting", note = Nothing }
    , modifiers =
        []
    }
  , { process = { name = "preparing for due diligence", note = Nothing }
    , modifiers =
        []
    }
  , { process = { name = "preparing for exit", note = Nothing }
    , modifiers =
        []
    }
  , { process = { name = "researching", note = Nothing }
    , modifiers =
        [ { name = "industry", note = Nothing }
        , { name = "constantly", note = Nothing }
        ]
    }
  , { process = { name = "running lean", note = Nothing }
    , modifiers =
        []
    }
  , { process = { name = "scaling", note = Just "external pressure: customers, revenue, etc" }
    , modifiers =
        [ { name = "quickly", note = Nothing }
        ]
    }
  , { process = { name = "setting up an operating agreement", note = Nothing }
    , modifiers =
        []
    }
  , { process = { name = "shutting down", note = Nothing }
    , modifiers =
        []
    }
  , { process = { name = "spinning out", note = Nothing }
    , modifiers =
        []
    }
  , { process = { name = "starting a company", note = Just "meaningfully distinct from 'starting a project' and 'incorporating'—this is the process of transitioning from project to business, if the interviewee doesn't specifically mention incorporating at the moment." }
    , modifiers =
        [ { name = "commerce", note = Nothing }
        , { name = "repeatedly", note = Nothing }
        ]
    }
  , { process = { name = "starting a project", note = Nothing}
    , modifiers =
        [ { name = "at an incubator", note = Nothing }
        , { name = "again", note = Nothing }
        ]
    }
  , { process = { name = "switching expense tracking", note = Nothing}
    , modifiers =
        [ { name = "to Propellor", note = Nothing }
        , { name = "to a (different) accountant", note = Nothing }
        ]
    }
  , { process = { name = "switching marketing tools", note = Nothing}
    , modifiers =
        [ { name = "to Hubspot", note = Nothing }
        ]
    }
  , { process = { name = "switching HR provider", note = Nothing}
    , modifiers =
        [ { name = "to Trinet", note = Nothing }
        ]
    }
  , { process = { name = "talking to founders", note = Nothing}
    , modifiers = []
    }
  , { process = { name = "testing", note = Nothing}
    , modifiers =
        [ { name = "MVP", note = Nothing }
        ]
    }
  , { process = { name = "quitting your job", note = Nothing}
    , modifiers =
        [ { name = "instability", note = Nothing }
        , { name = "with a backup plan", note = Nothing }
        ]
    }
  , { process = { name = "using blogs ", note = Nothing}
    , modifiers =
        [ { name = "written by VCs", note = Nothing }
        , { name = "conflicting info", note = Nothing }
        ]
    }
  , { process = { name = "using cap table management", note = Nothing}
    , modifiers =
        [ { name = "DIY", note = Nothing }
        , { name = "helpful", note = Nothing }
        , { name = "good record-keeping", note = Nothing }
        ]
    }
  , { process = { name = "using expense tracking", note = Nothing}
    , modifiers =
        [ { name = "DIY", note = Just "ie. proactively tracking expenses but without paid software/help. use only when they mention how they were tracking. cf. diy + expense tracking" }
        , { name = "Propellor", note = Nothing }
        , { name = "Quickbooks Online", note = Nothing }
        ]
    }
  , { process = { name = "using experience", note = Nothing}
    , modifiers =
        [ { name = "from business school", note = Nothing }
        ]
    }
  , { process = { name = "using google", note = Nothing}
    , modifiers = []
    }
  , { process = { name = "using hackernews", note = Nothing}
    , modifiers = []
    }
  , { process = { name = "using HR provider", note = Nothing}
    , modifiers =
        [ { name = "Insperity", note = Nothing }
        , { name = "PEO", note = Nothing }
        , { name = "Gusto", note = Nothing }
        , { name = "for payroll", note = Nothing }
        , { name = "Paychex", note = Nothing }
        ]
    }
  , { process = { name = "using a lawyer", note = Nothing}
    , modifiers =
        [ { name = "low quality", note = Nothing }
        , { name = "to manage equity", note = Nothing }
        ]
    }
  , { process = { name = "using marketing tools", note = Nothing}
    , modifiers =
        [ { name = "WordPress", note = Nothing }
        , { name = "Salesforce", note = Nothing }
        , { name = "Hubspot", note = Nothing }
        , { name = "poor fit", note = Nothing }
        ]
    }
  , { process = { name = "using network", note = Just "ie. the means"}
    , modifiers =
        [ { name = "school", note = Nothing }
        , { name = "legal", note = Nothing }
        , { name = "accelerator", note = Nothing }
        , { name = "for testing", note = Nothing }
        , { name = "for validation", note = Nothing }
        , { name = "investors", note = Nothing }
        , { name = "prior teammates", note = Nothing }
        , { name = "for resources", note = Nothing }
        , { name = "for lawyer", note = Nothing }
        , { name = "lawyer's", note = Nothing }
        , { name = "personal/family", note = Nothing } ]
    }
  , { process = { name = "using a PEO", note = Nothing}
    , modifiers =
        [ { name = "stuck", note = Nothing }
        , { name = "poor fit", note = Nothing }
        , { name = "cobbled together", note = Nothing }
        ]
    }
  , { process = { name = "validating an idea", note = Just "use when they have successfully validated an idea. if they disprove an idea, that's 'not validating an idea'" }
    , modifiers = []
    }
  , { process = { name = "wanting to work on a startup", note = Nothing}
    , modifiers =
        [ { name = "because of growth potential", note = Nothing }
        , { name = "small team", note = Nothing }
        ]
    }
  , { process = { name = "wasting money", note = Nothing}
    , modifiers =
        [ { name = "LLC", note = Nothing }
        , { name = "expensive", note = Nothing }
        ]
    }
  , { process = { name = "wasting time", note = Nothing}
    , modifiers = []
    }
  , { process = { name = "winning an award", note = Nothing}
    , modifiers = []
    }
  , { process = { name = "working at a big company", note = Nothing}
    , modifiers =
        [ { name = "Google", note = Nothing }
        ]
    }
  , { process = { name = "working on a project", note = Nothing}
    , modifiers =
        [ { name = "part-time/nights and weekends", note = Nothing }
        , { name = "at an incubator", note = Nothing }
        ]
    }
  ]
