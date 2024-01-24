filenames = ["arimax_0_epoch.txt","arimax_1_epoch.txt","arimax_2_epoch.txt","arimax_3_epoch.txt"]
header = """
        \\begin{table}[htbp]
            \centering
            \\begin{tabular}{|l|l|l|l|l|l|l|}
            \hline
            Segment            & Factor    & MAPE       & ME       & MAE     & MPE         & RMSE    \\\\ \hline
            """
footer = """
            \end{tabular}
            \caption{ARIMAX forecasting results for the structural NS factors.}
            \label{tab:structuralARIMAX}
        \end{table}
"""
with open("table.tex","w") as g:
    g.write(header)
    for sec,file in enumerate(filenames):
        g.write("\\multirow{4}{*}{" + str(sec) + "}")
        with open(file,'r') as f:
            lines = f.readlines()
            for i in range(12):
                y = float(lines[6*i].split(":")[1])
                mape = float(lines[6*i+1].split(":")[1])
                me = float(lines[6*i+2].split(":")[1])
                mae = float(lines[6*i+3].split(":")[1])
                mpe = float(lines[6*i+4].split(":")[1])
                rmse = float(lines[6*i+5].split(":")[1])

                g.write(f"&{y}&{mape}&{me}&{mae}&{mpe}&{rmse}\\\\" + " \\cline{2-7}\n")

    g.write(footer)
            
