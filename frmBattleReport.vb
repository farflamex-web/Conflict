

Public Class frmBattleReport

    Private rtbReport As New RichTextBox()

    Public Sub New()
        ' Initialize form
        Me.Text = "Battle Report"
        Me.Size = New Size(800, 600)
        Me.StartPosition = FormStartPosition.CenterParent

        ' Configure RichTextBox
        rtbReport.Dock = DockStyle.Fill
        rtbReport.ReadOnly = True
        rtbReport.Font = New Font("Consolas", 10)
        rtbReport.WordWrap = False   ' Important for horizontal scrolling
        rtbReport.ScrollBars = RichTextBoxScrollBars.Both

        ' Add to form
        Me.Controls.Add(rtbReport)
    End Sub

    ''' <summary>
    ''' Display the report text in the RichTextBox.
    ''' </summary>
    Public Sub ShowReport(reportText As String)
        rtbReport.Clear()
        rtbReport.AppendText(reportText)
        rtbReport.SelectionStart = 0
        rtbReport.ScrollToCaret()
        Me.Show()
    End Sub

End Class
