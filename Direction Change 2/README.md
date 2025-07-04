# Problem: Direction Change 2

You have a grid (lattice) of size **8×8**.  
You start at the **bottom-left** corner and your goal is to reach the **top-right** corner.  

- You may only move **right** or **up**; no downward or leftward moves are allowed.  
- In total you must take **8** rightward steps and **8** upward steps, in any order.  

Define a **direction change** as follows:  
- If one step is **right**, and the very next step is **up**, that counts as one direction change.  
- Likewise, an **up** step followed immediately by a **right** step also counts as one direction change.  

Thus, as you trace a path of 16 moves (8 rights + 8 ups), the total number of direction changes can be odd or even.

---

**Puzzle:**  
If you choose one of these valid paths uniformly at random, what is the **probability** that it has an **odd** number of direction changes?

---

**Source:**  
[YouTube: Citadel Interview question : Direction Change II by QuantProf](https://www.youtube.com/watch?v=PbmM-k00GiM)

[**Solution**](https://aquaregiaanubhav.github.io/Puzzles-in-R/direction-change.html)