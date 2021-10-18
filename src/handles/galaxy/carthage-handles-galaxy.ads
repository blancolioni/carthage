private with WL.Graphs;

with Ada.Streams;
with Carthage.Handles.Planets;

package Carthage.Handles.Galaxy is

   procedure Scan_Connections
     (From : Carthage.Handles.Planets.Planet_Handle;
      Process : not null access
        procedure (Planet : Carthage.Handles.Planets.Planet_Handle));

   function Connected
     (Planet_1, Planet_2 : Carthage.Handles.Planets.Planet_Handle)
      return Boolean;

   function Jump_Count
     (Planet_1, Planet_2 : Carthage.Handles.Planets.Planet_Handle)
      return Natural;

   function Jump_Route
     (Planet_1, Planet_2 : Carthage.Handles.Planets.Planet_Handle)
      return Carthage.Handles.Planets.Array_Of_Planets;

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class);

private

   function Index_Of
     (Planet : Carthage.Handles.Planets.Planet_Handle)
      return Real_Planet_Reference
   is (Planet.Reference);

   package Planet_Graph is
     new WL.Graphs
       (Index_Type   => Real_Planet_Reference,
        Vertex_Type  => Carthage.Handles.Planets.Planet_Handle,
        Cost_Type    => Non_Negative_Real,
        Default_Cost => 1.0,
        Index_Of     => Index_Of,
        "="          => Carthage.Handles.Planets."=");

   Graph : Planet_Graph.Graph;

   function Connected
     (Planet_1, Planet_2 : Carthage.Handles.Planets.Planet_Handle)
      return Boolean
   is (Graph.Connected (Planet_1.Reference, Planet_2.Reference));

   function Jump_Count
     (Planet_1, Planet_2 : Carthage.Handles.Planets.Planet_Handle)
      return Natural
   is (Graph.Path_Vertices
       (Graph.Shortest_Path (Planet_1.Reference, Planet_2.Reference))
         'Length);

end Carthage.Handles.Galaxy;
