import tkinter as tk
from tkinter import filedialog, font, colorchooser
import os
import re
import requests
import time
import os
import numpy as np

#dir_path = os.path.dirname(os.path.realpath(__file__))

root = tk.Tk()
root.title('COBOL')
root.iconbitmap('')
root.geometry("300x300")
root.grid_rowconfigure(0, weight=2,uniform=True)
root.grid_rowconfigure(1, weight=7,uniform=True)
root.grid_columnconfigure(0, weight=7,uniform=True)
root.grid_columnconfigure(1, weight=7,uniform=True)
root.grid_columnconfigure(2, weight=7,uniform=True)

######################################################################################
def translate(text):    
    while True:
        try:
            r = requests.get(f"https://translate.googleapis.com/translate_a/single?client=gtx&sl=FR&tl=EN&dt=t&q={text}")
            break
        except requests.exceptions.RequestException as e:
            print(e)
            time.sleep(2)
            continue
    if r.json()[0]:
        return r.json()[0][0][0]
    return None

#############################################################################

def parse(txt):
    text = txt[:]
    
    idx = np.array([(m.start(1), m.end(1)) for m in re.finditer(r"\s{6}\*(.*?)\n",text)])

    i = 0
    while i < idx.shape[0]:
        trans = translate(text[idx[i][0]:idx[i][1]])
        if trans:
            diff = len(text[idx[i][0]:idx[i][1]]) - len(trans)
            text = text[:idx[i][0]] + trans + text[idx[i][1]:]
            idx[i][1] -= diff
            idx[i+1:] -= diff
        i+=1
    return text

##########################################################################################

class commands:
    def __init__(self,root,logs_text):
        self.root = root
        self.src_path = None
        self.dist_path = None
        self.logs_text=logs_text

    def src_directory(self):
        self.src_path = filedialog.askdirectory()
        text = f"src directory: {self.src_path}\ndist directory: {self.dist_path}"
        self.logs_text.delete("1.0", tk.END)
        self.logs_text.insert(tk.END,text)

    def dist_directory(self):
        self.dist_path = filedialog.askdirectory()
        #self.root.title(self.dist_path)
        text = f"src directory: {self.src_path}\ndist directory: {self.dist_path}"
        self.logs_text.delete("1.0", tk.END)
        self.logs_text.insert(tk.END,text)

    def start(self):

        # path = f"{dir_path}/DATA_EN"
        # # if not os.path.exists(path):
        # os.makedirs(path,exist_ok=True)
        
        if self.src_path:
           if self.dist_path:
                for file in os.listdir(self.src_path):
                    if file.endswith(".cbl"):
                        self.logs_text.insert(tk.END,f"Translating: {file}")
                        print(f"Translating: {file}")
                        with open(f"{self.src_path}/{file}","r",encoding='latin-1') as f:
                            text = f.read()
                        trans = parse(text)
                        with open(f"{self.dist_path}/{file}","w") as f:
                            f.write(trans)



#################################################################################
logs_frame = tk.Frame(root, background="#1E1E1E", bd=1, relief="sunken")
logs_frame.grid(row=1,column=0,columnspan =3, sticky="nsew", padx=2, pady=2)

logs_vert_scroll = tk.Scrollbar(logs_frame,orient="vertical")
logs_vert_scroll.pack(side=tk.RIGHT, fill=tk.Y)

logs_hor_scroll = tk.Scrollbar(logs_frame,orient="horizontal")
logs_hor_scroll.pack(side=tk.BOTTOM, fill=tk.X)

logs_text = tk.Text(logs_frame,
                font=("Consolas",10),
                selectbackground="#007ACC",
                selectforeground="black",
                foreground="#9cdcfe",
                background="#1E1E1E",
                insertbackground="#D4D4D4",
                undo=True,
                yscrollcommand=logs_vert_scroll.set,
                wrap="none",
                xscrollcommand=logs_hor_scroll.set)
logs_text.pack(fill=tk.BOTH,expand=True)


logs_vert_scroll.config(command=logs_text.yview)
logs_hor_scroll.config(command=logs_text.xview)
##########################################################################################

cmd = commands(root=root,logs_text=logs_text)

status_bar = tk.Label(root, text='Ready        ',anchor=tk.E)

toolbar_frame = tk.Frame(root)
toolbar_frame.grid(row=0, column=0,columnspan =3, sticky="nsew", padx=2, pady=2)

toolbar_frame.grid_columnconfigure(0, weight=7,uniform=True)
toolbar_frame.grid_columnconfigure(1, weight=7,uniform=True)
toolbar_frame.grid_columnconfigure(2, weight=7,uniform=True)

src_button = tk.Button(toolbar_frame, text="Source", command=cmd.src_directory)
src_button.grid(row=0, column=0, sticky=tk.W,padx= 5)

dist_button = tk.Button(toolbar_frame, text="Dist", command=cmd.dist_directory)
dist_button.grid(row=0, column=1, sticky=tk.W,padx= 5)

trans_button = tk.Button(toolbar_frame, text="Translate", command=cmd.start)
trans_button.grid(row=0, column=2, sticky=tk.W,padx= 5)

root.mainloop()