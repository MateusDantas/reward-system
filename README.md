# Reward
Reward is a optimized referral system that takes a list of referrals and can output the ranked score of each user. A user score is defined as follows:

A user gets (1/2)^k points for each invitation that is directly or indirectly affected by it, where k is the level of the invitation. 
This means that if each direct invitation, will reward the user 1 point if this inivation has invited someone else. And each subsequent invitation will lead 0.5, 0.25, 0.125, ... and so on points
in case any of those invitees have invited someone else as well.

## Solution

This problem is a straight forward graph theorem problem. Each invitation can be represented as an edge of a tree. 
So we can calculate the user's score by looping through a user's children and applying the following calculation:
```
forEach chilfren of user do
if children has invited someone then
   userScore += 1 + (children score) / 2.0
```

I decided to have three structures:
- Tree:

 - Data: {nodes: Map key Node}, Node {key, metadata, children, parent}
 - Each node of the tree is mapped to its key, so we can have fast access to it if we want. A node is basically an object with its metadata, a list of keys to its children and a key to its parent. This allows us to do lookups and updates more fast.

- Forest: 

 - Data {trees: Map key Tree, nodeMap: Map key key} 

 - In order to have fast access to the trees contained within the forest, we store two things: ⋅⋅

  - trees: For each new tree we map its root key to its object, this way if we have the tree root we can access its object as fast as the map structure allows us.

  - nodeMap: For each node contained withing the forest, we map it to its tree root key, this way we can also have fast access to the representing tree of any node.

- Ranking: 

 - Data Forest
 - The ranking structure contains a forest with all the edges. It also holds the logic necessary to calculate the score of each node.

With this strucuture in mind, we add a new referral following the algorithm:
```
let referral = src dst
if forest doesn't have dst as a node then
  add referral to forest

forest = calculateScoreFrom src forest
```

This means that after adding a new referral (an edge), we go up in tree starting from 'src' node and update its score and its parents score respectively.
```
update node = updateScore of node then update (parent of node)
```


## Performance

The tree is implemented using maps (Data.IntMap.Strict), which has lookup, update and insertion close to O(min(n,32)) complexity. For particular large values of n, this means that we have O(1) complexity for lookup, update and insertion.

Given this, the performance of the following operations are:
- insertReferral: O(n) (for n > 32) -> When updating the tree score, we have to go all the way up to the root and  this operation takes O(n * log(n))
- queryUserScore: O(1) (for n > 32) -> Querying a user score is straightforward, we just find the tree its in and then we find the node inside this tree. 
- score: O(n) (for n > 32) -> Traversing a tree takes O(n * log(n)) time as we have to go through its children and make lookup queries along the way.

This implementation is ideal if we want to have fast querying for particular a particular user score. However, if we want to have fast insertion instead, we can easily change this implementation to allow for O(1) insertion. However, this means that querying for a specific user score would lead to a O(n) complexity.

## Endpoints

- GET - /ranking -> Returns a list of {key, score} sorted by score in descending order.
- GET - /ranking/:uid -> Returns an object of {key, score} for the user with id = :uid
- PUT - /ranking/:src/:dst -> Adds a new invitation from :src to :dst
- POST - /ranking/file -> Receives a file with a list of invitations and add it.

## How to run

Go to the root folder and run 'stack ghci', if its successful run 'main'.

## How to test

Unit tests were implemented using HUnit, in order to run then, just use the command 'stack test'.


