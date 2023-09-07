module Main exposing (main)

import Browser
import Html as H exposing (Html)
import Html.Attributes as HA
import Html.Events as HE
import Html.Keyed as HK
import Html.Lazy exposing (lazy)

import Array exposing (Array)
import Dict exposing (Dict)

-- This is the inflated version of the data, where all the school name references are resolved.
type alias Data = Dict String (List String)

data : Data
data = Dict.fromList <|
	[ ("CIST004A", ["UCSB", "UCSC"])
	, ("CIST005A", ["UCSC", "UCLA"])
	]

type alias Flags = ()
type alias Model =
	{ course_search: String
	, selected_course: String
	}
type Msg
	= CourseSearchChange String
	| SelectedCourseChange String

main : Program Flags Model Msg
main = Browser.document { init = init, update = update, subscriptions = subscriptions, view = view }

init : Flags -> (Model, Cmd Msg)
init _ =
	(
		{ course_search = ""
		, selected_course = ""
		}
	, Cmd.none
	)

update : Msg -> Model -> (Model, Cmd Msg)
update msg model = case msg of
	CourseSearchChange new -> ({ model | course_search = new, selected_course = if String.contains new model.selected_course then model.selected_course else List.head (filter_courses new) |> Maybe.withDefault "" }, Cmd.none)
	SelectedCourseChange new -> ({ model | selected_course = new }, Cmd.none)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

filter_courses : String -> List String
filter_courses search =
	Dict.keys data |> List.filter (String.contains search)

license : Html msg
license = H.footer [] <| List.singleton <| H.p [] <| List.singleton <| H.text "TODO advertise license and source code link"

view : Model -> Browser.Document Msg
view model = { title = "Assist Frontend", body =
	let
		search_courses = H.input [HA.placeholder "Course name", HA.value model.course_search, HE.onInput CourseSearchChange] []
		select_course = HK.node "select" [HA.value model.selected_course, HE.onInput SelectedCourseChange] <| ("", H.option [HA.value ""] [H.text "Select one"]) :: (List.map (\s -> (s, H.option [] [H.text s])) <| filter_courses model.course_search)
		course_info = lazy (\selected_course -> case Dict.get selected_course data of
			Nothing -> H.text ""
			Just schools -> H.div []
				[ H.p [] [H.text "This course is accepted at the following schools:"]
				, HK.node "ul" [] <| List.map (\school -> (school, H.li [] [H.text school])) schools
				]
			) model.selected_course

	in
		[ H.h1 [] [H.text "Assist Frontend"]
		, search_courses
		, select_course
		, course_info
		, license
		]
	}
