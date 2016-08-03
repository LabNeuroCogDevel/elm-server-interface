# Annotating Jason's work

File hierarchy

## Pages, Components, and Views


 * Pages are top level route display components intended to not be embeddable
 * Components are embeddable, have data.
 * Views do not have their own data. They just display.

 * Components: `Contacts`, `Search`, `Visits`
 * Pages: `Page`,`People`,`Studies`,`Visits`
 * Views: `Boostrap` `Pagination` `TabPane`
 * each have at least `Model`, `Update`, and `View`
 * Comp/Contacts, Comp/Visits, and all Pages also have `HttpCmds` (used with `Utils/Http` for crud)
 * Contacts has an additional `TableView`

TODO: 
 * Tasks ?


## Nav

Handle routing

## Types
 * subforlders contain JSON encoder and decoder. 
 * `Crud` for  `Utils/Http` and `Utils/Http/Handlers` (to message)

## Core
MUV for main page

## Utils
 * functions for dealing withJSON, date, http, list, navigation, etc 
