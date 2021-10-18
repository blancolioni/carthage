package body Carthage.Handles.Galaxy is

   ----------------
   -- Jump_Route --
   ----------------

   function Jump_Route
     (Planet_1, Planet_2 : Carthage.Handles.Planets.Planet_Handle)
      return Carthage.Handles.Planets.Array_Of_Planets
   is
      Vertices : constant Planet_Graph.Array_Of_Vertices :=
                   Graph.Path_Vertices
                     (Graph.Shortest_Path
                        (Planet_1.Reference, Planet_2.Reference));
   begin
      return Ps : Carthage.Handles.Planets.Array_Of_Planets (Vertices'Range) do
         for I in Ps'Range loop
            Ps (I) := Graph.Vertex (Vertices (I));
         end loop;
      end return;
   end Jump_Route;

   ----------
   -- Load --
   ----------

   procedure Load
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Planet_Graph.Graph'Read (Stream, Graph);
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save
     (Stream : not null access Ada.Streams.Root_Stream_Type'Class)
   is
   begin
      Planet_Graph.Graph'Write (Stream, Graph);
   end Save;

   ----------------------
   -- Scan_Connections --
   ----------------------

   procedure Scan_Connections
     (From : Carthage.Handles.Planets.Planet_Handle;
      Process : not null access
        procedure (Planet : Carthage.Handles.Planets.Planet_Handle))
   is
      procedure Process_Edge
        (Planet : Carthage.Handles.Planets.Planet_Handle;
         Cost   : Non_Negative_Real);

      ------------------
      -- Process_Edge --
      ------------------

      procedure Process_Edge
        (Planet : Carthage.Handles.Planets.Planet_Handle;
         Cost   : Non_Negative_Real)
      is
         pragma Unreferenced (Cost);
      begin
         Process (Planet);
      end Process_Edge;

   begin
      Graph.Iterate_Edges
        (From, Process_Edge'Access);
   end Scan_Connections;

end Carthage.Handles.Galaxy;
