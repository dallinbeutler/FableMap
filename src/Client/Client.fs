module Client

open Elmish
open Elmish.React
open Fable.React
open Fable.React.Props
open Fulma
open Leaflet
open Fable.Core.JsInterop


module RL = ReactLeaflet

importAll "../../node_modules/leaflet/dist/leaflet.css"

Leaflet.icon?Default?imagePath <- "//cdnjs.cloudflare.com/ajax/libs/leaflet/1.3.1/images/"
type Marker = { info: string; link: string; lat: float; lng:float; t:float}
type Model = { Markers: Marker list }

type Msg =
    | Tick
    | Nop

let llex (m:Marker) = 
    (Fable.Core.U3.Case3 (m.lat + (sin m.t)/4., m.lng + (cos m.t)/4.))

let initMarkers count = 
    seq {for  i in 0..count
            do for j in 0.. count
                do yield { info = "info"; 
                           link = "www.twitter.com"; 
                           lat = 54.425 + (float i/10.0 - 0.5); 
                           lng = 18.5 + (float j/10.0 - 0.5) ;
                           t = 0.0}
        }|>Seq.toList
    
let init () : Model * Cmd<Msg> = 
    // let initialModel = { Markers = [{ info = "info"; link = "www.twitter.com"; position = (Fable.Core.U3.Case3 (54.425, 18.59)) }]}
    let initialModel = { 
        Markers = initMarkers 20
        }
    initialModel, Cmd.none

let movePoint (markerList:Marker list) = 
    markerList |> List.map (fun p -> {p with t = p.t + 0.01})

    
    

let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    match msg with 
    |Tick -> {currentModel with Markers = movePoint currentModel.Markers },Cmd.none
    |Nop -> currentModel, Cmd.none  

let buildMarker (marker: Marker): ReactElement =
    RL.marker 
      [ 
         
        // RL.MarkerProps.Position ( LatLngExpression.ofLatLngTuple (marker.lat,marker.lng) )] 
        RL.MarkerProps.Position ( llex marker )] 
      [ RL.popup 
          [ RL.PopupProps.Key marker.link]
          [ Control.p 
              [] 
              [ label [] [ !!marker.info ] ]
            Control.p 
                [] 
                [ Button.a
                    [ Button.Size IsSmall
                      Button.Props [ Href (marker.link) ] ]
                    [ Icon.icon [ ]
                        [ ]
                      span [ ] [ str "Go to" ] ] ] ] ]     

let tile =
  RL.tileLayer 
    [ 
        
      RL.TileLayerProps.Url "https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png"
    //   RL.TileLayerProps.Url "https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"
      RL.TileLayerProps.Attribution "&amp;copy <a href=&quot;http://osm.org/copyright&quot;>OpenStreetMap</a> contributors" ] 
    []

let mapElements model =
  printfn "rebuilt markers"
  let markers = model.Markers |> List.map buildMarker
  tile :: markers    

let view (model : Model) (dispatch : Msg -> unit) =
    Hero.hero [ Hero.Color IsPrimary; Hero.IsFullHeight ]
        [ Hero.head [ ]
            []

          Hero.body [ ]
            [ Container.container [ Container.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                [ Column.column
                    [ Column.Width (Screen.All, Column.Is9)
                      Column.Offset (Screen.All, Column.Is1) ]
                    [ Heading.p [ ]
                        [ str "Leaflet example" ]
                      RL.map [ RL.MapProps.Animate true ;RL.MapProps.Zoom 8.; RL.MapProps.Style [Height 500; MinWidth 200; Width Column.IsFull ]; RL.MapProps.Center ( llex model.Markers.[0])  ] 
                        (mapElements model)
                                              
                       ] ] ] ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

let ticker (disp:Dispatch<Msg>) = 
  //Async.RunSynchronously ((AsyncRx.interval 10 10).SubscribeAsync(fun t -> async{disp (Tick)} ))
  let rec tick() =
    async {
        do! Async.Sleep 16
        disp Tick
        return! tick()
    }
  tick() |> Async.Start
  ()


Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
#endif
|> Program.withReactBatched "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.withSubscription (fun m -> Cmd.ofSub ticker )
|> Program.run