SELECT        MacroPlot.MacroPlot_Name, MacroPlot.MacroPlot_UTM_X, MacroPlot.MacroPlot_UTM_Y, SampleEvent.SampleEvent_Date, Cover_Points_metric_Sample.Visited, Cover_Points_metric_Attribute.[Index], 
                         Cover_Points_metric_Attribute.Transect, Cover_Points_metric_Attribute.Point, Cover_Points_metric_Attribute.[Tape], Cover_Points_metric_Attribute.[Order], Cover_Points_metric_Attribute.Height, 
                         Cover_Points_metric_Attribute.Spp_GUID, Cover_Points_metric_Attribute.CanopyLayer, Cover_Points_metric_Attribute.Status, Cover_Points_metric_Attribute.Comment
FROM            SampleEvent INNER JOIN
                         MacroPlot ON SampleEvent.SampleEvent_Plot_GUID = MacroPlot.MacroPlot_GUID INNER JOIN
                         Cover_Points_metric_Sample ON SampleEvent.SampleEvent_GUID = Cover_Points_metric_Sample.SampleData_SampleEvent_GUID INNER JOIN
                         Cover_Points_metric_Attribute ON Cover_Points_metric_Sample.SampleData_SampleRow_GUID = Cover_Points_metric_Attribute.AttributeData_SampleRow_GUID