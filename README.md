### Purpose

Use this repository to share code that is either too small or not ready for its own repository. The sections below include steps for sharing code that has not made it to the Christini Lab group before.

#### New Projects

##### Local

1. If you do not have the **code-share-112018** repo saved locally, then use the following code to clone it to your local machine:

```
git clone https://github.com/Christini-Lab/code-share-112018.git
```

2. Start at the root of the **code-share-112018** repository (the root is where you can find `README.md` file), and on master. You can check with branch you are on by typing:

```
git branch
```

3. If you are already on master, you can skip to step 3. If you are not on master, type the following:

```
git checkout master
```

4. Use the following code to create a new branch. Make the branch with a descriptive name, and use hyphens between words (i.e. `add-beeler-reuter-python`).

```
git checkout -b add-beeler-reuter-python
```

5. Copy your code into a folder in the root of this directory. The path to your code should be something like:

```
./beeler-reuter/script_br.py
```

**Note:** Your new folder can include multiple files &mdash; the above example is the path to only one file.

6. Use the code below to add and commit your changes to the current branch. The `git add .` command will add all changes to the current index. The `git commit -m "add message here"` command will commit the changes to the current branch. You should include a descriptive message with your commit (after the `-m` flag). Best pracitce is to start with an active verb. 

```
git add .
git commit -m "Add files for the beeler reuter python model"
```

7. Use the code below to push your changes to the remote repository. 

```
git push -u origin add-beeler-reuter-python
```
**Note:** Substitiute `add-beeler-reuter-python` with your own branch name.

##### Remote

1. In the **code-share-112018** repo, click on the Pull Request tab.
2. Click on the green New pull request button on the right side of your page.
3. You should see two drop-down selection boxes. Make sure the `base:` is set to `master` and the `compare:` is set to your branch name (i.e. `add-beeler-reuter-python`).
4. Once you've done that, select Create pull request.
5. Select Create pull request again. 

That's it. You can share the Pull Request URL with someone for review, or tag the person with their GitHub name in the conversation tab.











