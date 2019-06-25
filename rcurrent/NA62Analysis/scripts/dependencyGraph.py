

class Node:

    def __init__(self, name):
        self.name = name
        self.edges = []

    def addEdge(self, node):
        self.edges.append(node)


class DependencyGraph:

    def __init__(self):
        self.graph = {}
        self.resolved = []
        self.unresolved = []
        self.independent = []
        self.not_yet_resolved = []

    def addNode(self, nodeName):
        if nodeName not in self.graph:
            self.graph[nodeName] = []
            self.independent.append(nodeName)
            self.not_yet_resolved.append(nodeName)

    def removeNode(self, nodeName):
        del self.graph[nodeName]
        if self.independent.count(nodeName):
            self.independent.remove(nodeName)
        if self.not_yet_resolved.count(nodeName):
            self.not_yet_resolved.remove(nodeName)

    def addDependency(self, nodeName, dependencyName):
        # if node does not exist, add it
        if nodeName not in self.graph:
            self.addNode(nodeName)
        # if dependency node does not exist, add it
        if dependencyName not in self.graph:
            self.addNode(dependencyName)
        # add dependency to node
        self.graph[nodeName].append(dependencyName)
        # if dependency was independent, it's not anymore remove it from the list
        if self.independent.count(dependencyName):
            self.independent.remove(dependencyName)

    def removeDependency(self, nodeName, dependencyName):
        self.graph[nodeName].remove(dependencyName)
        if dependencyName in self.graph:
            self.removeNode(dependencyName)

    def getDependencies(self, nodeName):
        if nodeName in self.graph:
            return self.graph[nodeName]
        return []

    def resolve(self, node, ord_list):
        self.unresolved.append(node)
        for edge in self.getDependencies(node):
            if edge not in self.resolved:
                if edge in self.unresolved:
                    print "Circular dependency detected between %s and %s " % (node, edge)
                    return [-1]
                ord_list = self.resolve(edge, ord_list)
            if len(ord_list) == 1 and ord_list[0] == -1:
                return [-1]
        self.resolved.append(node)
        self.unresolved.remove(node)
        self.not_yet_resolved.remove(node)
        ord_list.append(node)
        return ord_list

    def getNextPath(self):
        self.unresolved = []

        if len(self.independent) == 0:
            if len(self.resolved) == len(self.graph):
                # all dependencies solved
                return []
            else:
                list = self.resolve(self.not_yet_resolved[0], [])
                return list

        list = self.resolve(self.independent.pop(), [])
        return list
