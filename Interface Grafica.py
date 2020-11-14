from tkinter import *

class Tkinter_Template(object):
  def __init__(self,master):
    frame = Frame(master)
    frame.grid()
    hello = Label(master,
      text = "Interface básica feita com Tkinter",
      font = ("Arial", 18))
    hello.grid()

if __name__ == '__main__':
  root = Tk()
  root.title("Projeto básico em Tkinter")
  root.geometry("600x80")
  Tkinter_Template(root)
  root.mainloop()
